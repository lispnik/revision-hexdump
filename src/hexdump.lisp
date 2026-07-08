;;;; hexdump.lisp --- the HEX-VIEW widget: an editable three-column hexdump.
;;;;
;;;; A byte buffer rendered as   OFFSET   HEX BYTES   ASCII   and edited in place.
;;;; The buffer is a fill-pointered (unsigned-byte 8) vector; editing OVERWRITES
;;;; the byte under the cursor, so the file keeps its size (the classic hex-editor
;;;; default).  Movement/edit/save are plain functions over the view, so they are
;;;; exercised headlessly in the test suite; DRAW and HANDLE-EVENT layer the UI on
;;;; top using the framework's public widget-authoring primitives.

(in-package #:revision-hexdump)

(defconstant +bpr+ 16 "Default bytes per row, until LAYOUT sizes the view to its width.")

;;; --- column geometry (pure; BPR = bytes per row, chosen from the window width) --
;;; A row is:  8-hex OFFSET  ·  BPR hex bytes (grouped in 8s by a wider gap)  ·  BPR
;;; ASCII chars.  Every column is derived from BPR so the three panes stay aligned.

(defun %off-w () 10)                                   ; "XXXXXXXX" + two spaces
(defun %hex-col (i)                                    ; column of byte I's first hex digit
  (+ (%off-w) (* i 3) (floor i 8)))                    ; +1 extra space after each group of 8
(defun %hex-end (bpr) (+ (%hex-col (1- bpr)) 2))       ; one past the last hex digit
(defun %ascii-col (bpr i) (+ (%hex-end bpr) 2 i))      ; two-space gap, then the ASCII gutter
(defun %row-w (bpr) (1+ (%ascii-col bpr (1- bpr))))    ; columns a full row occupies

(defun %col->byte (bpr col)
  "Map a view-local COLUMN to the byte it addresses: (values :hex I), (values
:ascii I), or (values NIL NIL) when the column is in no byte cell."
  (dotimes (i bpr (values nil nil))
    (let ((hc (%hex-col i)))
      (when (<= hc col (1+ hc)) (return (values :hex i)))
      (when (= col (%ascii-col bpr i)) (return (values :ascii i))))))

(defun %fit-bpr (width)
  "The largest bytes-per-row (a multiple of 8, at least 8) whose row fits WIDTH columns."
  (let ((best 8))
    (loop for b from 16 by 8 to 256 while (<= (%row-w b) width) do (setf best b))
    best))

;;; --- the byte buffer --------------------------------------------------------

(defun %make-buf (n)
  (make-array n :element-type '(unsigned-byte 8) :adjustable t :fill-pointer n))

(defun read-file-bytes (path)
  "Read PATH into a fresh fill-pointered (unsigned-byte 8) vector."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((buf (%make-buf (file-length s))))
      (read-sequence buf s)
      buf)))

(defun write-file-bytes (path bytes)
  "Write BYTES (a sequence of octets) to PATH, replacing it."
  (with-open-file (s path :element-type '(unsigned-byte 8)
                          :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-sequence bytes s))
  path)

;;; --- a paged source for large files -----------------------------------------
;;; A file over *MAX-IN-MEMORY* is not slurped into RAM; instead it is read one
;;; page at a time, on demand, through a bounded page cache -- so a multi-GB file
;;; can be viewed / navigated / searched without loading it.  A PIECE-TABLE (below)
;;; layers edits on top, so it is fully editable too; small files are loaded fully.

(defparameter *max-in-memory* (* 64 1024 1024)
  "Files larger than this (bytes) open read-only and paged, rather than loaded into RAM.")
(defparameter *fs-page-size* 65536 "Bytes read per page for a paged file source.")
(defparameter *fs-max-cache-pages* 256 "Cached pages before the cache is dropped (bounds memory).")

(defstruct (file-source (:constructor %make-fs) (:copier nil))
  stream length (page-size *fs-page-size*) (cache (make-hash-table)))

(defun %fs-read-page (fs pg)
  (let* ((ps (file-source-page-size fs)) (start (* pg ps))
         (n (max 0 (min ps (- (file-source-length fs) start))))
         (buf (make-array n :element-type '(unsigned-byte 8))))
    (file-position (file-source-stream fs) start)
    (read-sequence buf (file-source-stream fs))
    buf))

(defun fs-ref (fs i)
  "Byte I of the file, read through the page cache."
  (let* ((ps (file-source-page-size fs)) (pg (floor i ps)) (cache (file-source-cache fs)))
    (when (> (hash-table-count cache) *fs-max-cache-pages*) (clrhash cache))   ; bound memory
    (aref (or (gethash pg cache) (setf (gethash pg cache) (%fs-read-page fs pg)))
          (mod i ps))))

(defun fs-close (fs) (ignore-errors (close (file-source-stream fs))))

;;; A byte provider for the "other" file in a diff: read (paged if large) + length + closer.
(defstruct (diff-src (:constructor %diff-src) (:conc-name ds-)) ref length close)

(defun %open-diff-src (path)
  "Open PATH as a byte provider for diffing (paged when large, in-memory otherwise)."
  (let ((len (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))))
    (if (> len *max-in-memory*)
        (let ((fs (%make-fs :stream (open path :element-type '(unsigned-byte 8)) :length len)))
          (%diff-src :ref (lambda (i) (fs-ref fs i)) :length len :close (lambda () (fs-close fs))))
        (let ((vec (read-file-bytes path)))
          (%diff-src :ref (lambda (i) (aref vec i)) :length len :close nil)))))

;;; --- a piece table: editing a large file (incl. insert/delete) without loading it ---
;;; The document is a sequence of PIECES, each spanning either the original file
;;; (:orig, read through the page cache) or an append-only in-memory ADD buffer.
;;; Insert / delete / overwrite splice pieces, so a multi-GB file's *size* can change
;;; with no full copy in RAM; on save the pieces are streamed out in order.

(defstruct (piece-table (:constructor %make-piece-table) (:conc-name pt-) (:copier nil))
  source                                                ; the FILE-SOURCE (:orig reads)
  (add (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (pieces '())                                          ; list of (SRC START LEN), SRC :orig | :add
  (length 0)
  (cache nil))                                          ; (BASE . PIECES-TAIL) of the last read

(defun %make-pt (src)
  "A piece table presenting the whole of SRC (the file) as one :orig piece."
  (%make-piece-table :source src
                     :pieces (list (list :orig 0 (file-source-length src)))
                     :length (file-source-length src)))

(defun pt-ref (pt i)
  "Byte I of the document (through the page cache for :orig, the add buffer for :add).
A one-entry cache makes the common sequential scan O(1) per byte."
  (let* ((cache (pt-cache pt)) (use (and cache (>= i (car cache)))) (base (if use (car cache) 0)))
    (loop for tail on (if use (cdr cache) (pt-pieces pt))
          for p = (car tail) for len = (third p) do
            (when (< i (+ base len))
              (setf (pt-cache pt) (cons base tail))
              (let ((off (+ (second p) (- i base))))
                (return-from pt-ref
                  (if (eq (first p) :orig) (fs-ref (pt-source pt) off) (aref (pt-add pt) off)))))
            (incf base len))
    0))

(defun %pt-insert-piece (pieces pos new)
  "PIECES with NEW spliced in at offset POS (splitting a straddling piece)."
  (let ((base 0) (out '()) (done nil))
    (dolist (p pieces)
      (destructuring-bind (src start len) p
        (cond (done (push p out))
              ((= pos base) (push new out) (push p out) (setf done t))
              ((< pos (+ base len))
               (let ((left (- pos base)))
                 (push (list src start left) out) (push new out)
                 (push (list src (+ start left) (- len left)) out) (setf done t)))
              (t (push p out)))
        (incf base len)))
    (if done (nreverse out) (nreverse (cons new out)))))

(defun pt-insert (pt pos byte)
  "Insert one BYTE at offset POS.  Sequential typing coalesces into a single :add piece."
  (let ((add (pt-add pt)))
    (vector-push-extend (logand byte #xff) add)
    (let ((addpos (1- (fill-pointer add))) (base 0) (coalesced nil))
      (dolist (p (pt-pieces pt))
        (destructuring-bind (src start len) p
          (when (and (not coalesced) (= (+ base len) pos) (eq src :add) (= (+ start len) addpos))
            (setf (third p) (1+ (third p)) coalesced t))
          (incf base len)))
      (unless coalesced
        (setf (pt-pieces pt) (%pt-insert-piece (pt-pieces pt) pos (list :add addpos 1)))))
    (incf (pt-length pt))
    (setf (pt-cache pt) nil)))

(defun pt-delete (pt pos)
  "Delete the byte at offset POS."
  (let ((base 0) (out '()))
    (dolist (p (pt-pieces pt))
      (destructuring-bind (src start len) p
        (cond ((or (<= (+ base len) pos) (> base pos)) (push p out))   ; doesn't contain POS
              (t (let ((left (- pos base)))
                   (when (plusp left) (push (list src start left) out))
                   (let ((rlen (- len left 1)))
                     (when (plusp rlen) (push (list src (+ start left 1) rlen) out))))))
        (incf base len)))
    (setf (pt-pieces pt) (nreverse out) (pt-cache pt) nil)
    (decf (pt-length pt))))

(defun pt-set (pt pos byte)
  "Overwrite the byte at POS (delete then insert, since original file bytes are immutable)."
  (pt-delete pt pos)
  (pt-insert pt pos byte))

(defun pt-save (pt stream)
  "Write the whole document to STREAM in piece order, streaming :orig spans from the file."
  (dolist (p (pt-pieces pt))
    (destructuring-bind (src start len) p
      (if (eq src :add)
          (write-sequence (pt-add pt) stream :start start :end (+ start len))
          (loop with fss = (file-source-stream (pt-source pt))
                for off from start below (+ start len) by *fs-page-size*
                for n = (min *fs-page-size* (- (+ start len) off))
                for buf = (make-array n :element-type '(unsigned-byte 8))
                do (file-position fss off) (read-sequence buf fss) (write-sequence buf stream))))))

;;; --- the view ---------------------------------------------------------------

(defclass hex-view (view)
  ((bytes    :initarg :bytes :initform (%make-buf 0) :accessor hexv-bytes
             :documentation "The editable octet buffer (a fill-pointered (unsigned-byte 8) vector).")
   (cursor   :initform 0 :accessor hexv-cursor        ; byte offset under the cursor
             :documentation "Byte offset the cursor addresses.")
   (top      :initform 0 :accessor hexv-top)          ; first visible row (scroll offset)
   (nibble   :initform 0 :accessor hexv-nibble        ; 0 = high, 1 = low (hex pane only)
             :documentation "Which nibble of the current byte the hex pane will edit next (0 hi, 1 lo).")
   (pane     :initform :hex :accessor hexv-pane        ; :hex | :ascii
             :documentation "The active edit pane, :HEX or :ASCII (Tab toggles).")
   (mode     :initform :overwrite :accessor hexv-mode   ; :overwrite | :insert (Ins toggles)
             :documentation "Editing mode: :OVERWRITE keeps the file size; :INSERT lets typing insert bytes and Bksp/Del remove them.")
   (inspector :initform t :accessor hexv-inspector       ; data inspector shown at the foot of the view? (Ctrl-T)
             :documentation "Show the data-inspector panel (2 lines) inside the view?  Ctrl-T toggles.")
   (offset-decimal :initform nil :accessor hexv-offset-decimal  ; offset column in decimal? (Ctrl-B)
             :documentation "Show offsets in decimal instead of hex (Ctrl-B toggles).")
   (locked   :initform nil :accessor hexv-locked          ; a user read-only lock on an editable buffer (Ctrl-L)
             :documentation "A read-only lock on an otherwise-editable buffer (Ctrl-L); large files are always read-only.")
   (ctrl-glyphs :initform t :accessor hexv-ctrl-glyphs    ; render control bytes as Unicode pictures?
             :documentation "Render control bytes as Unicode control pictures (␀␁…␡) rather than '.' (Ctrl-P toggles).")
   (marks    :initform (make-hash-table) :accessor hexv-marks    ; bookmarked offsets (a set)
             :documentation "Bookmarked offsets: Ctrl-K toggles one at the cursor, Ctrl-N/Ctrl-P jump between them.")
   (fields   :initform nil :accessor hexv-fields         ; a vector of TFIELDs from an applied template, or NIL
             :documentation "The parsed leaf fields (a vector of TFIELDs) of an applied structural template, or NIL.")
   (template-name :initform nil :accessor hexv-template-name)   ; the applied template's name, for display
   (diff     :initform nil :accessor hexv-diff           ; a DIFF-SRC for the file being compared against, or NIL
             :documentation "A byte provider for the file this one is being diffed against; NIL when not diffing.")
   (diff-name :initform nil :accessor hexv-diff-name)    ; the compared file's name, for display
   (bpr      :initform +bpr+ :accessor hexv-bpr          ; bytes per row, chosen from the view width by LAYOUT
             :documentation "Bytes shown per row; LAYOUT sizes it to the view width (a multiple of 8).")
   (big-endian :initform nil :accessor hexv-big-endian  ; data-inspector byte order (NIL = little-endian)
             :documentation "Byte order the data inspector decodes with (NIL little-endian; Ctrl-E toggles).")
   (anchor   :initform nil :accessor hexv-anchor         ; selection anchor offset (NIL = no selection)
             :documentation "The offset where a Shift-extended selection began, or NIL when nothing is selected.")
   (source   :initform nil :accessor hexv-source          ; a FILE-SOURCE for a large paged file, else NIL
             :documentation "A paged FILE-SOURCE when a large file is open (not slurped into RAM); NIL for an in-memory buffer.")
   (pt       :initform nil :accessor hexv-pt               ; a PIECE-TABLE over the paged file (edits without loading it)
             :documentation "A PIECE-TABLE editing the paged large file (insert/delete/overwrite without loading it); NIL for an in-memory buffer.")
   (last-search :initform nil :accessor hexv-last-search)  ; the last search pattern (byte vector), for find-next
   (changed  :initform (make-hash-table) :accessor hexv-changed)  ; offset -> T, touched since load/save (highlight)
   (history  :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor hexv-history
             :documentation "The edit log: a vector of tagged records ((:SET off old new) / (:INS off val) / (:DEL off val)) for undo/redo.")
   (hpos     :initform 0 :accessor hexv-hpos            ; how many history edits are currently applied
             :documentation "Number of HISTORY edits applied to the buffer (the undo/redo position).")
   (saved-pos :initform 0 :accessor hexv-saved-pos      ; HPOS at the last load/save; -1 = clean state discarded
             :documentation "HPOS at the last load/save -- the clean checkpoint; -1 once it is unreachable.")
   (message  :initform nil :accessor hexv-message
             :documentation "A transient status note (save / go-to / undo feedback), shown until the next move.")
   (filename :initarg :filename :initform nil :accessor hexv-filename)
   (on-change :initarg :on-change :initform nil :accessor hexv-on-change
              :documentation "Optional thunk of the view, called after each edit."))
  (:metaclass reactive-class)
  (:documentation "An editable hexdump of a byte buffer: three aligned columns (offset · hex · ASCII)
that the cursor edits in place.  A reactive VIEW, so moving the cursor or editing a byte repaints;
it answers the scroll protocol so a hosting window draws a frame scrollbar."))

(defmethod focusable-p ((v hex-view)) (declare (ignore v)) t)

(defmethod layout ((v hex-view) rect)
  "Assign bounds and choose bytes-per-row from the available width, then keep the cursor
in view (a width change moves rows around)."
  (setf (view-bounds v) rect
        (hexv-bpr v) (%fit-bpr (rect-width rect)))
  (setf (hexv-top v) (min (hexv-top v) (scroll-max v)))
  (%ensure-visible v))

(defun hexv-length (v)
  (if (hexv-pt v) (pt-length (hexv-pt v)) (length (hexv-bytes v))))
(defun hexv-readonly (v) (hexv-locked v))               ; only the user lock disables editing
(defun hexv-resizable-p (v) (not (hexv-readonly v)))    ; insert/delete allowed (piece table for paged files)
(defun %bref (v i)
  "Byte I of V, whether it is an in-memory buffer or a paged file's piece table."
  (if (hexv-pt v) (pt-ref (hexv-pt v) i) (aref (hexv-bytes v) i)))
(defun %doc-set (v off byte)
  "Overwrite byte OFF, through the piece table for a paged file, else the in-memory buffer."
  (if (hexv-pt v) (pt-set (hexv-pt v) off byte) (setf (aref (hexv-bytes v) off) byte)))
(defun %close-source (v)
  "Close V's paged file source, if any."
  (when (hexv-source v) (fs-close (hexv-source v)) (setf (hexv-source v) nil (hexv-pt v) nil)))
(defun %ruler-rows (v) (declare (ignore v)) 1)          ; the column-header row at the top
(defun %inspector-rows (v) (if (hexv-inspector v) 2 0)) ; the data inspector at the foot
(defun %page (v)                                        ; scrollable dump rows (minus the chrome)
  (max 1 (- (rect-height (view-bounds v)) (%ruler-rows v) (%inspector-rows v))))
;; +1 so a cursor at the append position (offset = length, valid in insert mode) has a row.
(defun %rows (v) (max 1 (ceiling (1+ (hexv-length v)) (hexv-bpr v))))
(defun %max-cursor (v)
  "The greatest valid cursor offset: LENGTH in insert mode (the append position), else
LENGTH-1 (overwrite addresses an existing byte)."
  (let ((n (hexv-length v)))
    (if (eq (hexv-mode v) :insert) n (max 0 (1- n)))))

(defun hexv-modified (v)
  "Does V differ from the last saved/loaded state?  True iff the applied-edit position
has moved off the clean checkpoint (see HEXV-SAVED-POS)."
  (/= (hexv-hpos v) (hexv-saved-pos v)))

;;; --- load / save ------------------------------------------------------------

(defun hex-load (v path)
  "Load PATH into V, resetting the cursor and edit history.  A file larger than
*MAX-IN-MEMORY* opens read-only through a paged FILE-SOURCE (not slurped into RAM);
a smaller one is loaded fully and stays editable."
  (%close-source v)
  (let ((len (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))))
    (if (> len *max-in-memory*)
        (let ((src (%make-fs :stream (open path :element-type '(unsigned-byte 8)) :length len)))
          (setf (hexv-source v) src (hexv-pt v) (%make-pt src) (hexv-bytes v) (%make-buf 0)))
        (setf (hexv-source v) nil (hexv-pt v) nil (hexv-bytes v) (read-file-bytes path)))
    (setf (hexv-filename v) path
          (hexv-cursor v) 0 (hexv-top v) 0 (hexv-nibble v) 0 (hexv-anchor v) nil
          (fill-pointer (hexv-history v)) 0 (hexv-hpos v) 0 (hexv-saved-pos v) 0
          (hexv-message v) nil (hexv-fields v) nil (hexv-template-name v) nil))
  (%close-diff v)                                       ; a fresh file drops any active diff
  (clrhash (hexv-changed v))
  (invalidate v)
  v)

(defun %temp-sibling (path)
  "A temp pathname beside PATH (same directory, so a rename is atomic on one filesystem)."
  (make-pathname :name (concatenate 'string (or (pathname-name path) "file") "-hexdump-tmp")
                 :defaults path))

(defun %save-large (v path)
  "Stream the paged file's piece table to PATH -- via a sibling temp file, then a rename --
and reopen the source as a single :orig piece.  No full copy is held in RAM."
  (let* ((pt (hexv-pt v)) (newlen (pt-length pt)) (tmp (%temp-sibling path)))
    (with-open-file (out tmp :element-type '(unsigned-byte 8) :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
      (pt-save pt out))
    (fs-close (hexv-source v))
    (rename-file tmp path)
    (let ((src (%make-fs :stream (open path :element-type '(unsigned-byte 8)) :length newlen)))
      (setf (hexv-source v) src (hexv-pt v) (%make-pt src)))
    path))

(defun hex-save (v &optional (path (hexv-filename v)))
  "Write V to PATH (default its own filename) and mark it clean.  A paged large file is
streamed with its overlay applied (no RAM copy); a small buffer is written directly.  Returns
(values PATH NIL) on success, (values NIL ERROR) on failure, or (values NIL NIL) with no path.
Never signals -- a failed save must not crash the host loop."
  (cond
    ((null path) (values nil nil))
    (t (handler-case
           (progn (if (hexv-source v) (%save-large v path) (write-file-bytes path (hexv-bytes v)))
                  (setf (hexv-filename v) path
                        (hexv-saved-pos v) (hexv-hpos v))   ; this edit position is now the clean checkpoint
                  (clrhash (hexv-changed v))
                  (invalidate v)
                  (values path nil))
         (error (e) (values nil e))))))

;;; --- movement ---------------------------------------------------------------

(defun %ensure-visible (v)
  "Scroll so the cursor's row is on screen."
  (let ((row (floor (hexv-cursor v) (hexv-bpr v))) (page (%page v)))
    (cond ((< row (hexv-top v)) (setf (hexv-top v) row))
          ((>= row (+ (hexv-top v) page)) (setf (hexv-top v) (1+ (- row page)))))))

(defun %goto (v off)
  "Move the cursor to byte OFF (clamped to the valid range), reset the nibble, clear any
transient note, and reveal it."
  (setf (hexv-message v) nil)
  (when (or (plusp (hexv-length v)) (eq (hexv-mode v) :insert))
    (setf (hexv-cursor v) (max 0 (min off (%max-cursor v)))
          (hexv-nibble v) 0)
    (%ensure-visible v)))

(defun %move (v delta) (%goto v (+ (hexv-cursor v) delta)))

;;; --- editing --------------------------------------------------------------
;;; The buffer is a fill-pointered vector, so overwrite is an AREF, insert shifts
;;; the tail right and grows it, delete shifts left and shrinks it.  Every edit is
;;; logged as a tagged record so undo/redo can replay it in either direction.

(defun %buf-insert (v off value)
  "Insert VALUE (an octet) at OFF: into the piece table for a paged file, else the buffer."
  (if (hexv-pt v)
      (pt-insert (hexv-pt v) off value)
      (let ((buf (hexv-bytes v)))
        (vector-push-extend 0 buf)
        (loop for i from (1- (fill-pointer buf)) above off do (setf (aref buf i) (aref buf (1- i))))
        (setf (aref buf off) (logand value #xff)))))

(defun %buf-delete (v off)
  "Delete the byte at OFF: from the piece table for a paged file, else the buffer."
  (if (hexv-pt v)
      (pt-delete (hexv-pt v) off)
      (let ((buf (hexv-bytes v)))
        (loop for i from off below (1- (fill-pointer buf)) do (setf (aref buf i) (aref buf (1+ i))))
        (decf (fill-pointer buf)))))

(defun %push-edit (v edit)
  "Log EDIT at the current position: drop any redo tail, append, advance HPOS.  If the
clean checkpoint lived in the dropped tail, mark it unreachable."
  (setf (fill-pointer (hexv-history v)) (hexv-hpos v))
  (when (> (hexv-saved-pos v) (hexv-hpos v)) (setf (hexv-saved-pos v) -1))
  (vector-push-extend edit (hexv-history v))
  (incf (hexv-hpos v)))

(defun %apply-forward (v edit)
  (ecase (first edit)
    (:set (%doc-set v (second edit) (fourth edit)))
    (:ins (%buf-insert v (second edit) (third edit)))
    (:del (%buf-delete v (second edit)))
    (:group (dolist (sub (rest edit)) (%apply-forward v sub)))))

(defun %apply-reverse (v edit)
  (ecase (first edit)
    (:set (%doc-set v (second edit) (third edit)))
    (:ins (%buf-delete v (second edit)))
    (:del (%buf-insert v (second edit) (third edit)))
    (:group (dolist (sub (reverse (rest edit))) (%apply-reverse v sub)))))

(defun %edit-offset (edit)
  "A representative byte offset for EDIT (for revealing the cursor after undo/redo)."
  (if (eq (first edit) :group) (%edit-offset (second edit)) (second edit)))

(defun %coalesce (v start)
  "Replace the history entries added since START (an HPOS) with a single (:group ...) step,
so a multi-byte operation (paste, cut, replace) is one undo/redo."
  (let ((end (hexv-hpos v)))
    (when (> (- end start) 1)
      (let ((subs (loop for i from start below end collect (aref (hexv-history v) i))))
        (setf (fill-pointer (hexv-history v)) start)
        (vector-push-extend (cons :group subs) (hexv-history v))
        (setf (hexv-hpos v) (1+ start))))))

(defmacro %as-one-edit ((v) &body body)
  "Coalesce every edit BODY records on V into a single undo/redo step."
  (let ((vv (gensym "V")) (start (gensym "START")))
    `(let* ((,vv ,v) (,start (hexv-hpos ,vv)))
       (multiple-value-prog1 (progn ,@body)
         (%coalesce ,vv ,start)))))

(defun %after-edit (v &optional structural)
  "Shared bookkeeping after an edit: a STRUCTURAL (insert/delete) edit shifts offsets, so
drop the offset-keyed change highlights and any applied template; run the hook; repaint."
  (when structural
    (clrhash (hexv-changed v))
    (setf (hexv-fields v) nil (hexv-template-name v) nil))
  (when (hexv-on-change v) (funcall (hexv-on-change v) v))
  (invalidate v))

(defun %readonly-blocked (v)
  "True (with a status note) when V is read-only locked, so any edit is refused."
  (and (hexv-readonly v)
       (progn (setf (hexv-message v) "read-only (Ctrl-L to unlock)") (invalidate v) t)))

(defun %set-byte (v off value)
  "Overwrite byte OFF with VALUE (0-255), logging an undo step.  Works over an in-memory
buffer or a paged file's overlay.  A no-op write does nothing."
  (unless (hexv-readonly v)
    (let ((old (%bref v off)) (new (logand value #xff)))
      (unless (= old new)
        (%push-edit v (list :set off old new))
        (%doc-set v off new)
        (setf (gethash off (hexv-changed v)) t)
        (%after-edit v)))))

(defun %insert-byte (v off value)
  "Insert VALUE at OFF (insert mode; in-memory buffers only), logging an undo step."
  (when (hexv-resizable-p v)
    (%push-edit v (list :ins off (logand value #xff)))
    (%buf-insert v off value)
    (%after-edit v t)))

(defun %delete-byte (v off)
  "Delete the byte at OFF (if any; in-memory buffers only), logging an undo step."
  (when (and (hexv-resizable-p v) (< off (hexv-length v)))
    (%push-edit v (list :del off (%bref v off)))
    (%buf-delete v off)
    (setf (hexv-cursor v) (min (hexv-cursor v) (%max-cursor v)))
    (%after-edit v t)))

(defun hex-undo (v)
  "Undo the most recent edit; returns T when one was undone."
  (if (plusp (hexv-hpos v))
      (let ((edit (aref (hexv-history v) (decf (hexv-hpos v)))))
        (%apply-reverse v edit)
        (%after-edit v t)                                ; offsets may have shifted -> reset highlights
        (%goto v (%edit-offset edit))
        t)
      (progn (setf (hexv-message v) "nothing to undo") (invalidate v) nil)))

(defun hex-redo (v)
  "Reapply the next undone edit; returns T when one was redone."
  (if (< (hexv-hpos v) (fill-pointer (hexv-history v)))
      (let ((edit (aref (hexv-history v) (hexv-hpos v))))
        (incf (hexv-hpos v))
        (%apply-forward v edit)
        (%after-edit v t)
        (%goto v (%edit-offset edit))
        t)
      (progn (setf (hexv-message v) "nothing to redo") (invalidate v) nil)))

(defun hex-toggle-mode (v)
  "Toggle between overwrite and insert editing modes."
  (setf (hexv-mode v) (if (eq (hexv-mode v) :insert) :overwrite :insert)
        (hexv-nibble v) 0
        ;; leaving insert mode, drop the append position back onto a real byte
        (hexv-cursor v) (min (hexv-cursor v) (%max-cursor v)))
  (invalidate v))

(defun %parse-offset (s)
  "Parse a hex offset string (an optional 0x prefix, else plain hex), or NIL."
  (let ((str (string-trim " " s)))
    (when (plusp (length str))
      (let ((h (if (and (> (length str) 1) (char-equal (char str 0) #\0) (char-equal (char str 1) #\x))
                   (subseq str 2) str)))
        (ignore-errors (parse-integer h :radix 16))))))

(defun hex-prompt-goto (v)
  "Prompt for a hex offset and jump to it (an invalid entry is reported, not an error)."
  (let ((s (prompt-string " Go to offset " "Hex offset (e.g. 1F or 0x1F):")))
    (when s
      (let ((off (%parse-offset s)))
        (if off (%goto v off) (setf (hexv-message v) "invalid offset"))))
    (invalidate v)))

(defun %hx-say-saved (v path err)
  "Record the outcome of a save as V's status note."
  (setf (hexv-message v)
        (cond (path (format nil "saved ~D byte~:P to ~A" (hexv-length v) (file-namestring path)))
              (err  (format nil "save failed: ~A" err))
              (t    "save cancelled"))))

(defun hex-prompt-save-as (v)
  "Prompt for a destination with the framework's :save file dialog (which confirms an
overwrite) and save V there.  This is how a new, unnamed buffer gets its file."
  (when (%readonly-blocked v) (return-from hex-prompt-save-as))
  (let ((p (make-file-dialog :mode :save
                             :dir (if (hexv-filename v)
                                      (uiop:pathname-directory-pathname (hexv-filename v))
                                      *project-dir*)
                             :default-name (if (hexv-filename v)
                                               (file-namestring (hexv-filename v))
                                               "untitled.bin"))))
    (when p (multiple-value-call #'%hx-say-saved v (hex-save v p)))
    (invalidate v)))

(defun %hex-input (v digit)
  "Apply hex DIGIT (0-15) to the byte under the cursor.  Overwrite mode edits the current
byte's active nibble; insert mode's first nibble inserts a new byte (DIGIT as its high
nibble), the second finishes it.  Either way the low nibble advances the cursor."
  (when (%readonly-blocked v) (return-from %hex-input))
  (if (eq (hexv-mode v) :insert)
      (if (zerop (hexv-nibble v))
          (progn (%insert-byte v (hexv-cursor v) (ash digit 4))   ; new byte, high nibble
                 (setf (hexv-nibble v) 1))
          (progn (%set-byte v (hexv-cursor v)                     ; complete its low nibble
                            (logior (logand (%bref v (hexv-cursor v)) #xf0) digit))
                 (%move v 1)))
      (when (plusp (hexv-length v))
        (let* ((off (hexv-cursor v)) (b (%bref v off)))
          (if (zerop (hexv-nibble v))
              (progn (%set-byte v off (logior (ash digit 4) (logand b #x0f)))
                     (setf (hexv-nibble v) 1))
              (progn (%set-byte v off (logior (logand b #xf0) digit))
                     (%move v 1)))))))                  ; %move resets the nibble to hi

(defun %ascii-input (v char)
  "Insert (insert mode) or overwrite (overwrite mode) the current byte with CHAR's code,
then advance the cursor."
  (when (%readonly-blocked v) (return-from %ascii-input))
  (cond
    ((eq (hexv-mode v) :insert) (%insert-byte v (hexv-cursor v) (char-code char)) (%move v 1))
    ((plusp (hexv-length v))    (%set-byte v (hexv-cursor v) (char-code char)) (%move v 1))))

;;; --- search -----------------------------------------------------------------

(defun %parse-search (s)
  "Parse a search string into an octet vector: a leading / means the rest is literal ASCII;
otherwise it is hex bytes (spaces optional, even number of digits).  NIL if unparseable."
  (cond
    ((zerop (length s)) nil)
    ((char= (char s 0) #\/) (map '(vector (unsigned-byte 8)) #'char-code (subseq s 1)))
    (t (let ((h (remove #\Space s)))
         (when (and (plusp (length h)) (evenp (length h))
                    (every (lambda (c) (digit-char-p c 16)) h))
           (loop with out = (make-array (floor (length h) 2) :element-type '(unsigned-byte 8))
                 for i below (length out)
                 do (setf (aref out i) (parse-integer h :start (* i 2) :end (+ 2 (* i 2)) :radix 16))
                 finally (return out)))))))

(defun %find-bytes (v pattern start)
  "The first index >= START where PATTERN occurs in V's bytes, or NIL (no wrap).  Reads
through %BREF, so it scans an in-memory buffer or a paged file source alike."
  (let ((n (hexv-length v)) (m (length pattern)))
    (when (plusp m)
      (loop for i from (max 0 start) to (- n m)
            when (loop for j below m always (= (%bref v (+ i j)) (aref pattern j)))
              return i))))

(defun hex-search (v pattern &optional (from (1+ (hexv-cursor v))))
  "Move the cursor to the next occurrence of PATTERN (an octet vector) at or after FROM,
wrapping once to the start.  Returns the offset, or NIL when there is no match."
  (let ((hit (or (%find-bytes v pattern from)
                 (%find-bytes v pattern 0))))            ; wrap
    (when hit (%goto v hit))
    hit))

(defun hex-prompt-find (v)
  "Prompt for a search pattern and jump to the next match; an empty entry repeats the last
search (find-next).  Reports the outcome, and never signals."
  (let ((s (prompt-string " Find " "Hex bytes (deadbeef) or /text:")))
    (when s
      (let ((pat (if (and (zerop (length (string-trim " " s))) (hexv-last-search v))
                     (hexv-last-search v)
                     (%parse-search s))))
        (cond
          ((null pat) (setf (hexv-message v) "invalid search pattern"))
          (t (setf (hexv-last-search v) pat)
             (let ((hit (hex-search v pat)))
               (setf (hexv-message v) (if hit (format nil "found at 0x~X" hit) "not found")))))))
    (invalidate v)))

;;; --- file diff --------------------------------------------------------------
;;; Compare this file against another, byte for byte: differing bytes (and bytes
;;; past the other file's end) are highlighted, and Ctrl-A jumps to the next one.

(defun %diff-at (v i)
  "Does byte I of V differ from the compared file (or lie past its end)?"
  (let ((d (hexv-diff v)))
    (and d (or (>= i (ds-length d)) (/= (%bref v i) (funcall (ds-ref d) i))))))

(defun %scan-diff (v from)
  "The first differing offset at or after FROM, wrapping once to the start, or NIL."
  (let ((n (hexv-length v)))
    (or (loop for i from from below n when (%diff-at v i) return i)
        (loop for i from 0 below (min from n) when (%diff-at v i) return i))))

(defun %close-diff (v)
  (let ((d (hexv-diff v)))
    (when d (when (ds-close d) (ignore-errors (funcall (ds-close d))))
          (setf (hexv-diff v) nil (hexv-diff-name v) nil))))

(defun hex-clear-diff (v) (%close-diff v) (invalidate v))

(defun hex-diff (v path)
  "Compare V against the file at PATH byte for byte, and jump to the first difference."
  (%close-diff v)
  (setf (hexv-diff v) (%open-diff-src path) (hexv-diff-name v) (file-namestring path))
  (let ((hit (%scan-diff v 0)))
    (if hit (progn (%goto v hit) (setf (hexv-message v) (format nil "diff vs ~a — first at 0x~X" (hexv-diff-name v) hit)))
        (setf (hexv-message v) (format nil "identical to ~a" (hexv-diff-name v)))))
  (invalidate v))

(defun hex-next-diff (v)
  "Jump to the next differing byte (wrapping)."
  (if (null (hexv-diff v))
      (setf (hexv-message v) "no diff — Ctrl-O to compare a file")
      (let ((hit (%scan-diff v (1+ (hexv-cursor v)))))
        (if hit (progn (%goto v hit) (setf (hexv-message v) (format nil "diff at 0x~X" hit)))
            (setf (hexv-message v) "no differences"))))
  (invalidate v))

(defun hex-open-diff (v)
  "Toggle a diff: clear the current one, or pick a file to compare against."
  (if (hexv-diff v)
      (progn (%close-diff v) (setf (hexv-message v) "diff off") (invalidate v))
      (let ((p (make-file-dialog :dir *project-dir* :title " Compare against ")))
        (when p (hex-diff v p)))))

(defun hexv-diff-line (v)
  "A one-line diff status for the cursor, or NIL when not diffing."
  (when (hexv-diff v)
    (let ((i (hexv-cursor v)) (d (hexv-diff v)))
      (if (%diff-at v i)
          (format nil "diff vs ~a · 0x~2,'0X vs ~A · Ctrl-A: next" (hexv-diff-name v) (%bref v i)
                  (if (< i (ds-length d)) (format nil "0x~2,'0X" (funcall (ds-ref d) i)) "(past end)"))
          (format nil "diff vs ~a · same here · Ctrl-A: next" (hexv-diff-name v))))))

(defun hex-toggle-pane (v)
  "Switch between the hex and ASCII edit panes."
  (setf (hexv-pane v) (if (eq (hexv-pane v) :hex) :ascii :hex)
        (hexv-nibble v) 0)
  (invalidate v))

;;; --- data inspector ---------------------------------------------------------
;;; Decode the bytes at the cursor as the common integer and float types, in the
;;; view's current byte order.  A hosting window shows these; the decoding is here
;;; (in the widget) so it is reusable and unit-tested without a screen.

(defun %read-uint (v off nbytes big-endian)
  "Assemble NBYTES of V at OFF into an unsigned integer, or NIL when fewer than NBYTES
remain from OFF.  Reads through %BREF (works for a paged file source too)."
  (when (<= (+ off nbytes) (hexv-length v))
    (let ((val 0))
      (dotimes (k nbytes val)
        (let ((idx (if big-endian (+ off k) (+ off (- nbytes 1 k)))))
          (setf val (logior (ash val 8) (%bref v idx))))))))

(defun %to-signed (u nbits)
  "Reinterpret unsigned U (or NIL) as an NBITS two's-complement signed integer."
  (if (and u (logbitp (1- nbits) u)) (- u (ash 1 nbits)) u))

(defun %read-f32 (v off big-endian)
  (let ((u (%read-uint v off 4 big-endian)))
    (when u (ignore-errors (sb-kernel:make-single-float (%to-signed u 32))))))

(defun %read-f64 (v off big-endian)
  (let ((u (%read-uint v off 8 big-endian)))
    (when u (ignore-errors
             (sb-kernel:make-double-float (%to-signed (ash u -32) 32) (logand u #xFFFFFFFF))))))

(defun hexv-inspect (v)
  "Decode the bytes at the cursor as an alist of (LABEL . STRING): u8/i8/u16/i16/u32/i32/
u64/i64 and f32/f64, in the view's current byte order.  A type whose bytes run past the
buffer end shows as \"—\"."
  (let ((off (hexv-cursor v)) (be (hexv-big-endian v)))
    (flet ((u (n) (%read-uint v off n be))
           (fmt (x) (if x (princ-to-string x) "—"))
           (fmtf (x) (cond ((null x) "—")
                           ;; a NaN comparison would signal FLOATING-POINT-INVALID-OPERATION,
                           ;; so catch the non-finite cases before any arithmetic on X
                           ((sb-ext:float-nan-p x) "NaN")
                           ((sb-ext:float-infinity-p x) (if (minusp x) "-Inf" "+Inf"))
                           ((> (abs x) 1d16) (format nil "~,4E" x))
                           (t (format nil "~,6G" x)))))
      (let ((b8 (u 1)))
        (list (cons "u8"  (fmt b8))                  (cons "i8"  (fmt (%to-signed b8 8)))
              (cons "char" (cond ((null b8) "—") ((<= 32 b8 126) (string (code-char b8))) (t ".")))
              (cons "bin" (if b8 (format nil "~8,'0B" b8) "—"))
              (cons "u16" (fmt (u 2)))               (cons "i16" (fmt (%to-signed (u 2) 16)))
              (cons "u32" (fmt (u 4)))               (cons "i32" (fmt (%to-signed (u 4) 32)))
              (cons "u64" (fmt (u 8)))               (cons "i64" (fmt (%to-signed (u 8) 64)))
              (cons "f32" (fmtf (%read-f32 v off be)))
              (cons "f64" (fmtf (%read-f64 v off be))))))))

(defun hexv-inspect-lines (v)
  "Two compact inspector lines for the byte(s) at the cursor, prefixed with the byte order."
  (let ((a (hexv-inspect v)) (be (if (hexv-big-endian v) "BE" "LE")))
    (flet ((g (k) (cdr (assoc k a :test #'string=))))
      (list
       (format nil "~a  u8 ~a  i8 ~a  '~a' ~a   u16 ~a  i16 ~a"
               be (g "u8") (g "i8") (g "char") (g "bin") (g "u16") (g "i16"))
       (format nil "    u32 ~a  i32 ~a  u64 ~a  i64 ~a  f32 ~a  f64 ~a"
               (g "u32") (g "i32") (g "u64") (g "i64") (g "f32") (g "f64"))))))

(defun hex-toggle-endian (v)
  "Toggle the data inspector between little- and big-endian."
  (setf (hexv-big-endian v) (not (hexv-big-endian v)))
  (invalidate v))

;;; --- structural templates ---------------------------------------------------
;;; A template describes a binary layout as (NAME TYPE) field specs, optionally led
;;; by (:endian :big|:little).  TYPE is a scalar keyword (:u8 .. :i64, :f32/:f64),
;;; (:bytes N), (:string N), (:array ELEMTYPE N), or (:struct FIELD...).  Applying a
;;; template at an offset parses the bytes into a flat list of typed leaf fields,
;;; which annotate the dump (each field's bytes tint; the field at the cursor shows).

(defstruct (tfield (:constructor %tfield) (:conc-name tf-))
  path type offset size value note)                     ; NOTE: an enum/flags annotation, or NIL

(defvar *tenv* nil "Field short-name -> value, for resolving dynamic lengths and references.")

(defparameter *template-max-array* 65536
  "Cap on a template :ARRAY's element count, so a corrupt or crafted length field read from
an arbitrary file can't make parsing allocate unboundedly.")

;; forward declaration: DETECT-TEMPLATE / LOAD-TEMPLATES (below) reference *TEMPLATES*
;; before its full definition (the built-in templates, further down).
(defvar *templates*)

(defun %short-name (path)
  (let ((dot (position #\. path :from-end t))) (if dot (subseq path (1+ dot)) path)))

(defun %resolve-count (count)
  "A length that is a literal integer, a symbol naming a prior field, or (:ref NAME)."
  (cond ((integerp count) count)
        ((symbolp count) (or (and *tenv* (gethash (string-downcase (string count)) *tenv*)) 0))
        ((and (consp count) (eq (first count) :ref))
         (or (and *tenv* (gethash (string-downcase (string (second count))) *tenv*)) 0))
        (t 0)))

(defun %enum-note (value options)
  "A display note for VALUE from OPTIONS: :enum ((val . name)...) picks a name; :flags
((bit . name)...) joins the set bits' names; else NIL."
  (let ((enum (getf options :enum)) (flags (getf options :flags)))
    (cond (enum (cdr (assoc value enum :test #'eql)))
          ((and flags (integerp value))
           (let ((set (loop for (bit . name) in flags when (logtest value bit) collect name)))
             (when set (format nil "~{~a~^|~}" set))))
          (t nil))))

(defun %scalar-size (type)
  (case type ((:u8 :i8) 1) ((:u16 :i16) 2) ((:u32 :i32 :f32) 4) ((:u64 :i64 :f64) 8) (t 0)))

(defun %read-scalar (ref len offset type be)
  "Decode a scalar TYPE at OFFSET (BE = big-endian), or NIL if it runs past LEN."
  (let ((n (%scalar-size type)))
    (when (and (plusp n) (<= (+ offset n) len))
      (let ((u (let ((v 0))
                 (dotimes (k n v)
                   (setf v (logior (ash v 8) (funcall ref (if be (+ offset k) (+ offset (- n 1 k))))))))))
        (case type
          ((:u8 :u16 :u32 :u64) u)
          (:i8 (%to-signed u 8)) (:i16 (%to-signed u 16)) (:i32 (%to-signed u 32)) (:i64 (%to-signed u 64))
          (:f32 (ignore-errors (sb-kernel:make-single-float (%to-signed u 32))))
          (:f64 (ignore-errors (sb-kernel:make-double-float (%to-signed (ash u -32) 32) (logand u #xFFFFFFFF)))))))))

(defun %read-str-field (ref len offset n)
  (with-output-to-string (s)
    (loop for k below n for idx = (+ offset k) while (< idx len)
          for b = (funcall ref idx) while (plusp b)
          do (write-char (if (<= 32 b 126) (code-char b) #\.) s))))

(defun %read-bytes-field (ref len offset n)
  (with-output-to-string (s)
    (loop for k below n for idx = (+ offset k) while (< idx len)
          do (format s "~:[~; ~]~2,'0X" (plusp k) (funcall ref idx)))))

(defun %parse-into (path type options ref len offset be acc)
  "Parse field PATH of TYPE (with per-field OPTIONS) at OFFSET; return (values SIZE ACC) with
leaf TFIELDs pushed on ACC.  Scalar values bind *TENV* so later fields' lengths can refer to
them; a length may be an integer or a name (see %RESOLVE-COUNT)."
  (cond
    ((keywordp type)
     (let* ((sz (%scalar-size type)) (val (%read-scalar ref len offset type be)))
       (when (and val (member type '(:u8 :i8 :u16 :i16 :u32 :i32 :u64 :i64)))
         (setf (gethash (%short-name path) *tenv*) val))
       (values sz (cons (%tfield :path path :type type :offset offset :size sz
                                 :value val :note (%enum-note val options)) acc))))
    ((eq (first type) :bytes)
     (let ((n (%resolve-count (second type))))
       (values n (cons (%tfield :path path :type :bytes :offset offset :size n
                                :value (%read-bytes-field ref len offset n)) acc))))
    ((eq (first type) :string)
     (let ((n (%resolve-count (second type))))
       (values n (cons (%tfield :path path :type :string :offset offset :size n
                                :value (%read-str-field ref len offset n)) acc))))
    ((eq (first type) :array)
     (destructuring-bind (elem n) (rest type)
       ;; clamp to [0, *template-max-array*]: a negative count (a signed length field) is
       ;; empty, and a crafted huge one is bounded so parsing can't OOM
       (let ((count (max 0 (min (%resolve-count n) *template-max-array*))) (total 0))
         (dotimes (i count (values total acc))
           (multiple-value-bind (sz a) (%parse-into (format nil "~a[~d]" path i) elem nil ref len (+ offset total) be acc)
             (setf total (+ total sz) acc a))))))
    ((eq (first type) :struct)
     (let ((total 0))
       (dolist (f (rest type) (values total acc))
         (destructuring-bind (fname ftype &rest fopts) f
           (multiple-value-bind (sz a)
               (%parse-into (format nil "~a.~(~a~)" path fname) ftype fopts ref len (+ offset total) be acc)
             (setf total (+ total sz) acc a))))))
    (t (values 0 acc))))

(defun parse-template (template ref len offset)
  "Parse TEMPLATE against bytes read via (funcall REF i) over [0,LEN), starting at OFFSET;
return a flat list of leaf TFIELDs in file order.  A field spec is (NAME TYPE . OPTIONS)."
  (let ((be nil) (fields template) (total 0) (acc '()) (*tenv* (make-hash-table :test 'equal)))
    (loop while (and (consp (first fields)) (keywordp (caar fields)))   ; skip leading options (:endian / :magic)
          do (when (eq (caar fields) :endian) (setf be (eq (second (first fields)) :big)))
             (setf fields (rest fields)))
    (dolist (f fields)
      (destructuring-bind (name type &rest options) f
        (multiple-value-bind (sz a)
            (%parse-into (string-downcase (string name)) type options ref len (+ offset total) be acc)
          (setf total (+ total sz) acc a))))
    (nreverse acc)))

(defun %template-magic (template)
  "The magic byte list a template declares via a leading (:magic BYTES) option, or NIL.
BYTES may be a string (its char codes) or a literal list of octets."
  (loop for f in template while (and (consp f) (keywordp (car f)))
        when (eq (car f) :magic)
          return (let ((m (second f))) (if (stringp m) (map 'list #'char-code m) m))))

(defun %magic-matches-p (magic ref len offset)
  (loop for b in magic for k from 0
        always (and (< (+ offset k) len) (= (funcall ref (+ offset k)) b))))

(defun detect-template (ref len offset)
  "The name of the first *TEMPLATES* entry whose :magic matches the bytes at OFFSET, or NIL."
  (loop for (name . specs) in *templates*
        for magic = (%template-magic specs)
        when (and magic (%magic-matches-p magic ref len offset)) return name))

(defun load-templates (path)
  "Read template definitions from PATH -- each top-level form a (NAME SPEC...) entry -- and
add them to *TEMPLATES* (replacing same-named ones).  Returns the count loaded, or NIL."
  (ignoring-errors ("load-templates")
    (with-open-file (s path :if-does-not-exist nil)
      (when s
        (loop with n = 0
              for form = (read s nil :eof) until (eq form :eof)
              when (and (consp form) (stringp (first form)))
                do (setf *templates* (cons form (remove (first form) *templates* :key #'first :test #'string=)))
                   (incf n)
              finally (return n))))))

(defun %tf-type-label (tf)
  (case (tf-type tf)
    (:bytes  (format nil "bytes~d" (tf-size tf)))
    (:string (format nil "str~d" (tf-size tf)))
    (t (string-downcase (string (tf-type tf))))))

(defun %tf-value-str (tf)
  (let ((v (tf-value tf)))
    (concatenate 'string
                 (cond ((null v) "—")
                       ((eq (tf-type tf) :string) (format nil "~s" v))
                       ((eq (tf-type tf) :bytes) v)
                       ((and (floatp v) (sb-ext:float-nan-p v)) "NaN")
                       ((and (floatp v) (sb-ext:float-infinity-p v)) (if (minusp v) "-Inf" "+Inf"))
                       (t (princ-to-string v)))
                 (if (tf-note tf) (format nil " [~a]" (tf-note tf)) ""))))

(defun %tf-str (tf)
  (format nil "~A @0x~X (~A) = ~A" (tf-path tf) (tf-offset tf) (%tf-type-label tf) (%tf-value-str tf)))

(defparameter *templates*
  '(("BMP file header"
     (:endian :little) (:magic "BM")
     (signature (:string 2)) (file-size :u32) (reserved :u32) (pixel-offset :u32)
     (dib-size :u32) (width :i32) (height :i32) (planes :u16) (bpp :u16))
    ("WAV (RIFF) header"
     (:endian :little) (:magic "RIFF")
     (riff (:string 4)) (chunk-size :u32) (wave (:string 4)) (fmt (:string 4)) (fmt-size :u32)
     (audio-format :u16) (channels :u16) (sample-rate :u32) (byte-rate :u32)
     (block-align :u16) (bits-per-sample :u16))
    ("GIF header"
     (:endian :little) (:magic "GIF8")
     (signature (:string 3)) (version (:string 3)) (width :u16) (height :u16)
     (flags :u8 :flags ((#x80 . "gct") (#x08 . "sort"))) (bg-color :u8) (aspect :u8))
    ("Length-prefixed record"     ; demonstrates dynamic length + enum + flags
     (:endian :little)
     (name-len :u8)
     (name (:string name-len))    ; length comes from the NAME-LEN field
     (kind  :u8  :enum ((0 . "none") (1 . "file") (2 . "dir")))
     (flags :u16 :flags ((#x1 . "active") (#x2 . "hidden") (#x4 . "system")))
     (tag-count :u8)
     (tags (:array :u16 tag-count))))   ; array length comes from TAG-COUNT
  "Built-in structural templates: (NAME . FIELD-SPECS...).  Applied at the cursor.  A field
spec is (NAME TYPE . OPTIONS); a length may name a prior field; :enum / :flags annotate.")

(defun hex-apply-template (v template name)
  "Parse TEMPLATE at the cursor and annotate the dump with its fields."
  (setf (hexv-fields v) (coerce (parse-template template (lambda (i) (%bref v i))
                                                (hexv-length v) (hexv-cursor v))
                                'vector)
        (hexv-template-name v) name)
  (invalidate v))

(defun hex-clear-template (v)
  (setf (hexv-fields v) nil (hexv-template-name v) nil) (invalidate v))

(defun hex-detect (v)
  "Detect a template by its magic bytes at the cursor and apply it; returns the name or NIL."
  (let ((name (detect-template (lambda (i) (%bref v i)) (hexv-length v) (hexv-cursor v))))
    (when name (hex-apply-template v (cdr (assoc name *templates* :test #'string=)) name))
    name))

(defun %field-at (v off)
  "The applied-template field spanning OFF, or NIL."
  (when (hexv-fields v)
    (loop for tf across (hexv-fields v)
          when (and (>= off (tf-offset tf)) (< off (+ (tf-offset tf) (tf-size tf)))) return tf)))

(defun hexv-field-line (v)
  "A one-line description of the template field at the cursor, or NIL."
  (let ((tf (%field-at v (hexv-cursor v)))) (and tf (concatenate 'string "field: " (%tf-str tf)))))

(defun hex-prompt-template (v)
  "Choose a structural template (or auto-detect / load-from-file / none) and apply it."
  (let ((choice (popup-choose (list* "(auto-detect)" "(load file…)" "(none)" (mapcar #'first *templates*))
                              :title " Apply template ")))
    (when choice
      (cond
        ((string= choice "(none)") (hex-clear-template v))
        ((string= choice "(auto-detect)")
         (let ((name (hex-detect v)))
           (setf (hexv-message v) (if name (format nil "detected ~a" name) "no template matched the bytes here"))))
        ((string= choice "(load file…)")
         (let ((p (make-file-dialog :dir *project-dir* :title " Load template file ")))
           (when p (let ((n (load-templates p)))
                     (setf (hexv-message v) (if n (format nil "loaded ~d template~:P" n) "could not load templates"))))))
        (t (let ((tmpl (cdr (assoc choice *templates* :test #'string=))))
             (when tmpl
               (hex-apply-template v tmpl choice)
               (setf (hexv-message v) (format nil "applied ~a (~d fields)" choice (length (hexv-fields v)))))))))
    (invalidate v)))

(defun hex-field-list (v)
  "Pick a field from the applied template and jump the cursor to it."
  (if (null (hexv-fields v))
      (setf (hexv-message v) "no template — Ctrl-J to apply one")
      (let* ((strs (map 'list #'%tf-str (hexv-fields v)))
             (choice (popup-choose strs :title (format nil " ~a " (hexv-template-name v)))))
        (when choice
          (let ((i (position choice strs :test #'string=)))
            (when i (%goto v (tf-offset (aref (hexv-fields v) i))))))))
  (invalidate v))

;;; --- selection + clipboard --------------------------------------------------

(defvar *clipboard* (make-array 0 :element-type '(unsigned-byte 8))
  "Shared byte clipboard for hex-view copy / cut / paste.")

(defun hexv-selection (v)
  "The inclusive (LO . HI) selected byte range, or NIL when nothing is selected."
  (let ((a (hexv-anchor v)))
    (when a (cons (min a (hexv-cursor v)) (max a (hexv-cursor v))))))

(defun %subbytes (v start end)
  "A fresh octet vector of V's bytes [START, END), read through %BREF."
  (let ((out (make-array (- end start) :element-type '(unsigned-byte 8))))
    (dotimes (k (- end start) out) (setf (aref out k) (%bref v (+ start k))))))

(defun %sel-anchor (v shift)
  "Before a move: extend the selection when SHIFT is held (setting the anchor on the first
extend), or collapse it otherwise."
  (if shift
      (unless (hexv-anchor v) (setf (hexv-anchor v) (hexv-cursor v)))
      (setf (hexv-anchor v) nil)))

(defun hex-copy (v)
  "Copy the selected bytes to the shared clipboard."
  (let ((sel (hexv-selection v)))
    (when sel
      (setf *clipboard* (%subbytes v (car sel) (1+ (cdr sel)))
            (hexv-message v) (format nil "copied ~D byte~:P" (length *clipboard*)))
      (invalidate v))))

(defun hex-cut (v)
  "Copy the selection to the clipboard and delete it (one undo step)."
  (when (%readonly-blocked v) (return-from hex-cut))
  (let ((sel (hexv-selection v)))
    (when sel
      (setf *clipboard* (%subbytes v (car sel) (1+ (cdr sel))))
      (%as-one-edit (v) (loop repeat (1+ (- (cdr sel) (car sel))) do (%delete-byte v (car sel))))
      (setf (hexv-anchor v) nil (hexv-message v) (format nil "cut ~D byte~:P" (length *clipboard*)))
      (%goto v (car sel))
      (invalidate v))))

(defun hex-paste (v)
  "Paste the clipboard at the cursor: insert (insert mode) or overwrite up to the buffer
end (overwrite mode).  One undo step."
  (when (%readonly-blocked v) (return-from hex-paste))
  (when (plusp (length *clipboard*))
    (let ((clip *clipboard*) (off (hexv-cursor v)))
      (if (eq (hexv-mode v) :insert)
          (%as-one-edit (v) (dotimes (k (length clip)) (%insert-byte v (+ off k) (aref clip k))))
          (%as-one-edit (v) (dotimes (k (length clip))
                              (when (< (+ off k) (hexv-length v))
                                (%set-byte v (+ off k) (aref clip k))))))
      (setf (hexv-anchor v) nil (hexv-message v) (format nil "pasted ~D byte~:P" (length clip)))
      (invalidate v))))

;;; --- find + replace ---------------------------------------------------------

(defun %replace-range (v off m replacement)
  "Replace the M bytes at OFF with the sequence REPLACEMENT (overwrite the overlap, then
insert extra / delete surplus)."
  (let ((r (length replacement)))
    (dotimes (k (min m r)) (%set-byte v (+ off k) (elt replacement k)))
    (cond ((> r m) (loop for k from m below r do (%insert-byte v (+ off k) (elt replacement k))))
          ((< r m) (loop repeat (- m r) do (%delete-byte v (+ off r)))))))

(defun hex-replace-all (v pattern replacement)
  "Replace every occurrence of PATTERN with REPLACEMENT; returns the count.  One undo step."
  (%as-one-edit (v)
    (let ((count 0) (m (length pattern)) (r (length replacement)))
      (loop with off = 0
            for hit = (%find-bytes v pattern off)
            while hit
            do (%replace-range v hit m replacement) (incf count) (setf off (+ hit r))
            finally (return count)))))

(defun hex-prompt-replace (v)
  "Prompt for a search pattern and a replacement, replace all occurrences, and report the
count.  An empty replacement deletes the matches."
  (when (%readonly-blocked v) (return-from hex-prompt-replace))
  (let ((s (prompt-string " Replace " "Find (hex bytes or /text):")))
    (when (and s (plusp (length (string-trim " " s))))
      (let ((pat (%parse-search s)))
        (if (null pat)
            (setf (hexv-message v) "invalid search pattern")
            (let ((rs (prompt-string " Replace " "With (hex, /text, or empty to delete):")))
              (when rs
                (let ((rep (if (zerop (length (string-trim " " rs)))
                               (make-array 0 :element-type '(unsigned-byte 8))
                               (%parse-search rs))))
                  (if (null rep)
                      (setf (hexv-message v) "invalid replacement")
                      (let ((n (hex-replace-all v pat rep)))
                        (setf (hexv-anchor v) nil
                              (hexv-message v) (format nil "replaced ~D occurrence~:P" n))))))))))
    (invalidate v)))

;;; --- the scroll protocol (a hosting window draws the frame scrollbar) --------

(defmethod scroll-page ((v hex-view)) (%page v))
(defmethod scroll-pos  ((v hex-view)) (hexv-top v))
(defmethod scroll-max  ((v hex-view)) (max 0 (- (%rows v) (%page v))))
(defmethod scroll-to   ((v hex-view) pos)
  (setf (hexv-top v) (max 0 (min pos (scroll-max v))))
  (invalidate v))

(defmethod frame-indicator ((v hex-view))
  (format nil " ~(~a~) ~a 0x~X/0x~X~:[~; *~] "
          (hexv-pane v)
          (cond ((hexv-readonly v) "ro") ((eq (hexv-mode v) :insert) "ins") (t "ovr"))
          (hexv-cursor v) (hexv-length v) (hexv-modified v)))

;;; --- drawing ----------------------------------------------------------------

(defun %cell-attr (v off cur-pane sel fld)
  "Colour for byte OFF in the pane CUR-PANE: cursor cell, selection (SEL is (LO . HI) or
NIL), a bookmark, the current template field (FLD is (LO . HI-exclusive) or NIL), an
edited-but-unsaved byte, or plain."
  (let ((cur (= off (hexv-cursor v))))
    (cond ((and cur (eq (hexv-pane v) cur-pane)) (role :focused))
          (cur                                   (role :input-focused))
          ((and sel (<= (car sel) off (cdr sel))) (role :menu-selected))  ; selection
          ((and (hexv-diff v) (%diff-at v off))  (role :error))           ; differs from the compared file
          ((gethash off (hexv-marks v))          (role :label))           ; bookmark
          ((and fld (<= (car fld) off) (< off (cdr fld))) (role :input))  ; current template field
          ((and (hexv-modified v) (gethash off (hexv-changed v))) (role :error))  ; flagged only while dirty
          (t                                     (role :normal)))))

(defun %ascii-glyph (v byte)
  "The ASCII-gutter character for BYTE: the printable char, else a control picture (␀..␡,
a middle dot for high bytes) or a plain '.' when control glyphs are off."
  (cond ((<= 32 byte 126)         (code-char byte))
        ((not (hexv-ctrl-glyphs v)) #\.)
        ((< byte 32)              (code-char (+ #x2400 byte)))   ; ␀ .. ␟
        ((= byte 127)             (code-char #x2421))            ; ␡
        (t                        (code-char #xB7))))            ; · for 128..255

(defun %fmt-offset (v off)
  (if (hexv-offset-decimal v) (format nil "~8,'0D" off) (format nil "~8,'0X" off)))

(defun %draw-ruler (v bpr w)
  "Draw the column-header row: the byte-column indices over the hex + ASCII panes."
  (let ((attr (role :frame-inactive)))
    (fill-row v 0 0 w attr)
    (draw-text v 0 0 (if (hexv-offset-decimal v) " (dec)  " " (hex)  ") attr)
    (dotimes (i bpr)
      (draw-text v (%hex-col i) 0 (format nil "~2,'0X" i) attr)
      (draw-text v (%ascii-col bpr i) 0 (string (digit-char (mod i 16) 16)) attr))))

(defun %draw-inspector (v h w)
  (when (hexv-inspector v)
    (loop for line in (hexv-inspect-lines v) for r from (- h 2)
          do (fill-row v 0 r w (role :normal))
             (draw-text v 1 r line (role :label)))))

(defmethod draw ((v hex-view))
  (let* ((b (view-bounds v)) (w (rect-width b)) (h (rect-height b))
         (ax (rect-ax b)) (ay (rect-ay b)) (bpr (hexv-bpr v))
         (n (hexv-length v)) (top (hexv-top v)) (sel (hexv-selection v))
         (page (%page v))
         (cf (%field-at v (hexv-cursor v)))              ; the template field under the cursor
         (fld (and cf (cons (tf-offset cf) (+ (tf-offset cf) (tf-size cf))))))
    (dotimes (r h) (fill-row v 0 r w (role :normal)))
    (%draw-ruler v bpr w)                                ; row 0
    (dotimes (dr page)                                   ; dump rows 1 .. page
      (let ((sr (1+ dr)) (base (* (+ top dr) bpr)))
        (when (< base (max 1 n))                         ; an empty file still shows its offset row
          (draw-text v 0 sr (%fmt-offset v base) (role :label))
          (dotimes (i bpr)
            (let ((off (+ base i)))
              (when (< off n)
                (let ((byte (%bref v off)))
                  (draw-text v (%hex-col i) sr (format nil "~2,'0X" byte) (%cell-attr v off :hex sel fld))
                  (draw-text v (%ascii-col bpr i) sr (string (%ascii-glyph v byte))
                             (%cell-attr v off :ascii sel fld)))))))))
    (%draw-inspector v h w)                              ; foot
    ;; a real block cursor in the active pane (only when focused); in insert mode it can
    ;; sit at the append position (offset = length), including an empty buffer.
    (when (and (view-focused-p v) *screen* (or (plusp n) (eq (hexv-mode v) :insert)))
      (let* ((off (hexv-cursor v)) (dr (- (floor off bpr) top)) (col (mod off bpr)))
        (when (<= 0 dr (1- page))                        ; cursor within the visible dump
          (let ((cx (if (eq (hexv-pane v) :hex)
                        (+ (%hex-col col) (hexv-nibble v))
                        (%ascii-col bpr col))))
            (when (< cx w)
              (set-cursor-pos *screen* (+ ax cx) (+ ay (1+ dr)))
              (set-cursor-shape :block)
              (show-cursor *screen*))))))))

;;; --- input ------------------------------------------------------------------

(defun %edit-key-p (mods)                               ; a plain edit key (Shift ok for A-F / uppercase)
  (not (logtest mods (logior +md-ctrl+ +md-alt+))))

(defun %ctrl-char-p (ks mods ch)
  (and (characterp ks) (char-equal ks ch) (logtest mods +md-ctrl+)))

(defun %hx-save-report (v)
  "Ctrl-S: save in place, or prompt for a name (Save-As) when the buffer is unnamed."
  (cond ((hexv-readonly v) (setf (hexv-message v) "read-only (Ctrl-L to unlock)") (invalidate v))
        ((hexv-filename v) (multiple-value-call #'%hx-say-saved v (hex-save v)) (invalidate v))
        (t (hex-prompt-save-as v))))

(defmethod handle-event ((v hex-view) (e key-event))
  (let* ((ks (event-keysym e)) (mods (event-modifiers e)) (shift (logtest mods +md-shift+)))
    (cond
      ((eql ks :left)  (%sel-anchor v shift) (%move v -1) (setf (handled-p e) t))
      ((eql ks :right) (%sel-anchor v shift) (%move v 1)  (setf (handled-p e) t))
      ((eql ks :up)    (%sel-anchor v shift) (%move v (- (hexv-bpr v))) (setf (handled-p e) t))
      ((eql ks :down)  (%sel-anchor v shift) (%move v (hexv-bpr v))     (setf (handled-p e) t))
      ((eql ks :pgup)  (%sel-anchor v shift) (%move v (- (* (hexv-bpr v) (%page v)))) (setf (handled-p e) t))
      ((eql ks :pgdn)  (%sel-anchor v shift) (%move v (* (hexv-bpr v) (%page v)))     (setf (handled-p e) t))
      ((eql ks :home)  (%sel-anchor v shift) (%goto v 0)                (setf (handled-p e) t))
      ((eql ks :end)   (%sel-anchor v shift) (%goto v (%max-cursor v))  (setf (handled-p e) t))
      ((eql ks :ins)   (hex-toggle-mode v)              (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\s) (%hx-save-report v)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\w) (hex-prompt-save-as v) (setf (handled-p e) t))  ; write-as
      ((%ctrl-char-p ks mods #\z) (hex-undo v)          (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\y) (hex-redo v)          (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\g) (hex-prompt-goto v)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\f) (hex-prompt-find v)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\r) (hex-prompt-replace v) (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\e) (hex-toggle-endian v) (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\c) (hex-copy v)          (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\x) (hex-cut v)           (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\v) (hex-paste v)         (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\t) (hex-toggle-inspector v)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\b) (hex-toggle-offset-base v) (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\u) (hex-toggle-ctrl-glyphs v) (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\l) (hex-toggle-lock v)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\k) (hex-toggle-mark v)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\n) (hex-next-mark v 1)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\p) (hex-next-mark v -1)  (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\d) (hex-prompt-template v) (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\q) (hex-field-list v)    (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\o) (hex-open-diff v)     (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\a) (hex-next-diff v)     (setf (handled-p e) t))
      ;; insert mode: Del removes the byte under the cursor, Bksp the one before it
      ((and (eq (hexv-mode v) :insert) (eql ks :del))
       (%delete-byte v (hexv-cursor v))                 (setf (handled-p e) t))
      ((and (eq (hexv-mode v) :insert) (eql ks :back))
       (let ((c (hexv-cursor v)))
         (when (plusp c) (%delete-byte v (1- c)) (%goto v (1- c))))
       (setf (handled-p e) t))
      ((and (eq (hexv-pane v) :hex) (characterp ks) (digit-char-p ks 16) (%edit-key-p mods))
       (%hex-input v (digit-char-p ks 16))              (setf (handled-p e) t))
      ((and (eq (hexv-pane v) :ascii) (characterp ks) (graphic-char-p ks)
            (< (char-code ks) 128) (%edit-key-p mods))
       (%ascii-input v ks)                              (setf (handled-p e) t))
      (t (call-next-method)))))                         ; Tab (pane, at the window), Esc/q bubble

(defmethod handle-event ((v hex-view) (e mouse-down))
  (let ((sr (mouse-row v e)))
    (when (plusp sr)                                     ; row 0 is the ruler
      (let ((base (* (+ (hexv-top v) (1- sr)) (hexv-bpr v))))
        (multiple-value-bind (pane i) (%col->byte (hexv-bpr v) (mouse-col v e))
          (when (and pane (< (+ base i) (hexv-length v)))
            (setf (hexv-pane v) pane (hexv-cursor v) (+ base i) (hexv-nibble v) 0
                  (hexv-anchor v) nil)                   ; a click collapses any selection
            (invalidate v))))))
  (setf (handled-p e) t))

;;; --- view toggles + bookmarks -----------------------------------------------

(defun hex-toggle-inspector (v)
  "Show / hide the data-inspector panel (reclaiming its two rows for the dump)."
  (setf (hexv-inspector v) (not (hexv-inspector v)))
  (%ensure-visible v) (invalidate v))
(defun hex-toggle-offset-base (v)
  (setf (hexv-offset-decimal v) (not (hexv-offset-decimal v))) (invalidate v))
(defun hex-toggle-ctrl-glyphs (v)
  (setf (hexv-ctrl-glyphs v) (not (hexv-ctrl-glyphs v))) (invalidate v))
(defun hex-toggle-lock (v)
  "Toggle a read-only lock (no effect on an already-read-only large file)."
  (if (hexv-source v)
      (setf (hexv-message v) "already read-only (large file)")
      (setf (hexv-locked v) (not (hexv-locked v))
            (hexv-message v) (if (hexv-locked v) "read-only lock ON" "read-only lock OFF")))
  (invalidate v))

(defun hex-toggle-mark (v)
  "Toggle a bookmark at the cursor."
  (let ((off (hexv-cursor v)))
    (cond ((gethash off (hexv-marks v))
           (remhash off (hexv-marks v)) (setf (hexv-message v) (format nil "mark cleared at 0x~X" off)))
          (t (setf (gethash off (hexv-marks v)) t) (setf (hexv-message v) (format nil "mark set at 0x~X" off))))
    (invalidate v)))

(defun %sorted-marks (v) (sort (loop for k being the hash-keys of (hexv-marks v) collect k) #'<))

(defun hex-next-mark (v &optional (dir 1))
  "Jump to the next (DIR 1) or previous (DIR -1) bookmark, wrapping around."
  (let ((marks (%sorted-marks v)) (cur (hexv-cursor v)))
    (if (null marks)
        (setf (hexv-message v) "no bookmarks — Ctrl-K sets one")
        (let ((target (if (plusp dir)
                          (or (find-if (lambda (m) (> m cur)) marks) (first marks))
                          (or (find-if (lambda (m) (< m cur)) (reverse marks)) (car (last marks))))))
          (%goto v target)
          (setf (hexv-message v) (format nil "mark 0x~X (~D total)" target (length marks)))))
    (invalidate v)))

(defmethod handle-event ((v hex-view) (e wheel-event))
  (scroll-to v (+ (hexv-top v) (* 3 (event-delta e))))
  (setf (handled-p e) t))

;;; widget-intrinsic keys, declared as data for the keybinding reference (#4)
(defmethod view-key-hints ((v hex-view))
  (declare (ignore v))
  '(("Left / Right" . "move one byte")
    ("Up / Down"    . "move one row")
    ("Shift+move"   . "extend the byte selection")
    ("PgUp / PgDn"  . "page up / down")
    ("Home / End"   . "start / end of file")
    ("Tab"          . "switch the hex / ASCII pane")
    ("Insert"       . "toggle overwrite / insert mode")
    ("0-9 a-f"      . "edit the byte's nibbles (hex pane)")
    ("(printable)"  . "edit the byte (ASCII pane)")
    ("Bksp / Del"   . "delete a byte (insert mode)")
    ("Ctrl+C / Ctrl+X / Ctrl+V" . "copy / cut / paste the selection")
    ("Ctrl+F"       . "find hex bytes or /text (empty = find next)")
    ("Ctrl+R"       . "replace all (hex or /text)")
    ("Ctrl+G"       . "go to a hex offset")
    ("Ctrl+K"       . "toggle a bookmark at the cursor")
    ("Ctrl+N / Ctrl+P" . "jump to the next / previous bookmark")
    ("Ctrl+D"       . "apply a structural template at the cursor")
    ("Ctrl+Q"       . "list the template's fields (jump to one)")
    ("Ctrl+O"       . "diff against another file (toggle)")
    ("Ctrl+A"       . "jump to the next difference")
    ("Ctrl+T"       . "show / hide the data inspector")
    ("Ctrl+E"       . "toggle the inspector's byte order (LE/BE)")
    ("Ctrl+B"       . "toggle the offset base (hex / decimal)")
    ("Ctrl+U"       . "toggle control-character glyphs")
    ("Ctrl+L"       . "toggle a read-only lock")
    ("Ctrl+Z / Ctrl+Y" . "undo / redo")
    ("Ctrl+S"       . "save (prompts for a name when the buffer is new)")
    ("Ctrl+W"       . "save as… (choose a new file)")))
