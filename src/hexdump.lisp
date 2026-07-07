;;;; hexdump.lisp --- the HEX-VIEW widget: an editable three-column hexdump.
;;;;
;;;; A byte buffer rendered as   OFFSET   HEX BYTES   ASCII   and edited in place.
;;;; The buffer is a fill-pointered (unsigned-byte 8) vector; editing OVERWRITES
;;;; the byte under the cursor, so the file keeps its size (the classic hex-editor
;;;; default).  Movement/edit/save are plain functions over the view, so they are
;;;; exercised headlessly in the test suite; DRAW and HANDLE-EVENT layer the UI on
;;;; top using the framework's public widget-authoring primitives.

(in-package #:revision-hexdump)

(defconstant +bpr+ 16 "Bytes shown per row.")

;;; --- column geometry (pure; the tests pin these) ----------------------------
;;; A row is:  8-hex OFFSET  ·  16 hex bytes (a wider gap splits the two groups of
;;; 8)  ·  16 ASCII chars.  Every column is derived from +BPR+ so the three panes
;;; stay aligned.

(defun %off-w () 10)                                   ; "XXXXXXXX" + two spaces
(defun %hex-col (i)                                    ; column of byte I's first hex digit
  (+ (%off-w) (* i 3) (if (>= i (floor +bpr+ 2)) 1 0)))  ; +1 splits the low/high groups of 8
(defun %hex-end () (+ (%hex-col (1- +bpr+)) 2))        ; one past the last hex digit
(defun %ascii-col (i) (+ (%hex-end) 2 i))              ; two-space gap, then the ASCII gutter
(defun %row-w () (1+ (%ascii-col (1- +bpr+))))         ; total columns a full row occupies

(defun %col->byte (col)
  "Map a view-local COLUMN to the byte it addresses: (values :hex I), (values
:ascii I), or (values NIL NIL) when the column is in no byte cell."
  (dotimes (i +bpr+ (values nil nil))
    (let ((hc (%hex-col i)))
      (when (<= hc col (1+ hc)) (return (values :hex i)))
      (when (= col (%ascii-col i)) (return (values :ascii i))))))

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

(defun hexv-length (v) (length (hexv-bytes v)))
(defun %page (v) (max 1 (rect-height (view-bounds v))))
;; +1 so a cursor at the append position (offset = length, valid in insert mode) has a row.
(defun %rows (v) (max 1 (ceiling (1+ (hexv-length v)) +bpr+)))
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
  "Load PATH's bytes into V, resetting the cursor and the whole edit history."
  (setf (hexv-bytes v) (read-file-bytes path)
        (hexv-filename v) path
        (hexv-cursor v) 0 (hexv-top v) 0 (hexv-nibble v) 0
        (fill-pointer (hexv-history v)) 0 (hexv-hpos v) 0 (hexv-saved-pos v) 0
        (hexv-message v) nil)
  (clrhash (hexv-changed v))
  (invalidate v)
  v)

(defun hex-save (v &optional (path (hexv-filename v)))
  "Write V's buffer to PATH (default its own filename) and mark it clean.  Returns
(values PATH NIL) on success, (values NIL ERROR) when the write failed, or (values NIL NIL)
when there is no path.  It never signals -- a failed save must not crash the host loop."
  (cond
    ((null path) (values nil nil))
    (t (handler-case
           (progn (write-file-bytes path (hexv-bytes v))
                  (setf (hexv-filename v) path
                        (hexv-saved-pos v) (hexv-hpos v))   ; this edit position is now the clean checkpoint
                  (clrhash (hexv-changed v))
                  (invalidate v)
                  (values path nil))
         (error (e) (values nil e))))))

;;; --- movement ---------------------------------------------------------------

(defun %ensure-visible (v)
  "Scroll so the cursor's row is on screen."
  (let ((row (floor (hexv-cursor v) +bpr+)) (page (%page v)))
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
  "Insert VALUE (an octet) into V's buffer at OFF, shifting the tail right."
  (let ((buf (hexv-bytes v)))
    (vector-push-extend 0 buf)
    (loop for i from (1- (fill-pointer buf)) above off do (setf (aref buf i) (aref buf (1- i))))
    (setf (aref buf off) (logand value #xff))))

(defun %buf-delete (v off)
  "Delete the byte at OFF from V's buffer, shifting the tail left."
  (let ((buf (hexv-bytes v)))
    (loop for i from off below (1- (fill-pointer buf)) do (setf (aref buf i) (aref buf (1+ i))))
    (decf (fill-pointer buf))))

(defun %push-edit (v edit)
  "Log EDIT at the current position: drop any redo tail, append, advance HPOS.  If the
clean checkpoint lived in the dropped tail, mark it unreachable."
  (setf (fill-pointer (hexv-history v)) (hexv-hpos v))
  (when (> (hexv-saved-pos v) (hexv-hpos v)) (setf (hexv-saved-pos v) -1))
  (vector-push-extend edit (hexv-history v))
  (incf (hexv-hpos v)))

(defun %apply-forward (v edit)
  (ecase (first edit)
    (:set (setf (aref (hexv-bytes v) (second edit)) (fourth edit)))
    (:ins (%buf-insert v (second edit) (third edit)))
    (:del (%buf-delete v (second edit)))))

(defun %apply-reverse (v edit)
  (ecase (first edit)
    (:set (setf (aref (hexv-bytes v) (second edit)) (third edit)))
    (:ins (%buf-delete v (second edit)))
    (:del (%buf-insert v (second edit) (third edit)))))

(defun %after-edit (v &optional structural)
  "Shared bookkeeping after an edit: a STRUCTURAL (insert/delete) edit shifts offsets, so
drop the offset-keyed change highlights; run the on-change hook; repaint."
  (when structural (clrhash (hexv-changed v)))
  (when (hexv-on-change v) (funcall (hexv-on-change v) v))
  (invalidate v))

(defun %set-byte (v off value)
  "Overwrite byte OFF with VALUE (0-255), logging an undo step.  A no-op write does nothing."
  (let ((old (aref (hexv-bytes v) off)) (new (logand value #xff)))
    (unless (= old new)
      (%push-edit v (list :set off old new))
      (setf (aref (hexv-bytes v) off) new (gethash off (hexv-changed v)) t)
      (%after-edit v))))

(defun %insert-byte (v off value)
  "Insert VALUE at OFF (insert mode), logging an undo step."
  (%push-edit v (list :ins off (logand value #xff)))
  (%buf-insert v off value)
  (%after-edit v t))

(defun %delete-byte (v off)
  "Delete the byte at OFF (if any), logging an undo step, and keep the cursor in range."
  (when (< off (hexv-length v))
    (%push-edit v (list :del off (aref (hexv-bytes v) off)))
    (%buf-delete v off)
    (setf (hexv-cursor v) (min (hexv-cursor v) (%max-cursor v)))
    (%after-edit v t)))

(defun hex-undo (v)
  "Undo the most recent edit; returns T when one was undone."
  (if (plusp (hexv-hpos v))
      (let ((edit (aref (hexv-history v) (decf (hexv-hpos v)))))
        (%apply-reverse v edit)
        (%after-edit v t)                                ; offsets may have shifted -> reset highlights
        (%goto v (second edit))
        t)
      (progn (setf (hexv-message v) "nothing to undo") (invalidate v) nil)))

(defun hex-redo (v)
  "Reapply the next undone edit; returns T when one was redone."
  (if (< (hexv-hpos v) (fill-pointer (hexv-history v)))
      (let ((edit (aref (hexv-history v) (hexv-hpos v))))
        (incf (hexv-hpos v))
        (%apply-forward v edit)
        (%after-edit v t)
        (%goto v (second edit))
        t)
      (progn (setf (hexv-message v) "nothing to redo") (invalidate v) nil)))

(defun hex-toggle-mode (v)
  "Toggle between overwrite and insert editing modes."
  (setf (hexv-mode v) (if (eq (hexv-mode v) :insert) :overwrite :insert)
        (hexv-nibble v) 0)
  ;; leaving insert mode, drop the append position back onto a real byte
  (setf (hexv-cursor v) (min (hexv-cursor v) (%max-cursor v)))
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

(defun %hex-input (v digit)
  "Apply hex DIGIT (0-15) to the byte under the cursor.  Overwrite mode edits the current
byte's active nibble; insert mode's first nibble inserts a new byte (DIGIT as its high
nibble), the second finishes it.  Either way the low nibble advances the cursor."
  (if (eq (hexv-mode v) :insert)
      (if (zerop (hexv-nibble v))
          (progn (%insert-byte v (hexv-cursor v) (ash digit 4))   ; new byte, high nibble
                 (setf (hexv-nibble v) 1))
          (progn (%set-byte v (hexv-cursor v)                     ; complete its low nibble
                            (logior (logand (aref (hexv-bytes v) (hexv-cursor v)) #xf0) digit))
                 (%move v 1)))
      (when (plusp (hexv-length v))
        (let* ((off (hexv-cursor v)) (b (aref (hexv-bytes v) off)))
          (if (zerop (hexv-nibble v))
              (progn (%set-byte v off (logior (ash digit 4) (logand b #x0f)))
                     (setf (hexv-nibble v) 1))
              (progn (%set-byte v off (logior (logand b #xf0) digit))
                     (%move v 1)))))))                  ; %move resets the nibble to hi

(defun %ascii-input (v char)
  "Insert (insert mode) or overwrite (overwrite mode) the current byte with CHAR's code,
then advance the cursor."
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

(defun %find-bytes (buf pattern start)
  "The first index >= START where PATTERN occurs in BUF, or NIL (no wrap)."
  (let ((n (length buf)) (m (length pattern)))
    (when (plusp m)
      (loop for i from (max 0 start) to (- n m)
            when (loop for j below m always (= (aref buf (+ i j)) (aref pattern j)))
              return i))))

(defun hex-search (v pattern &optional (from (1+ (hexv-cursor v))))
  "Move the cursor to the next occurrence of PATTERN (an octet vector) at or after FROM,
wrapping once to the start.  Returns the offset, or NIL when there is no match."
  (let* ((buf (hexv-bytes v))
         (hit (or (%find-bytes buf pattern from)
                  (%find-bytes buf pattern 0))))          ; wrap
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

(defun hex-toggle-pane (v)
  "Switch between the hex and ASCII edit panes."
  (setf (hexv-pane v) (if (eq (hexv-pane v) :hex) :ascii :hex)
        (hexv-nibble v) 0)
  (invalidate v))

;;; --- the scroll protocol (a hosting window draws the frame scrollbar) --------

(defmethod scroll-page ((v hex-view)) (%page v))
(defmethod scroll-pos  ((v hex-view)) (hexv-top v))
(defmethod scroll-max  ((v hex-view)) (max 0 (- (%rows v) (%page v))))
(defmethod scroll-to   ((v hex-view) pos)
  (setf (hexv-top v) (max 0 (min pos (scroll-max v))))
  (invalidate v))

(defmethod frame-indicator ((v hex-view))
  (format nil " ~(~a~) ~:[ovr~;ins~] 0x~X/0x~X~:[~; *~] "
          (hexv-pane v) (eq (hexv-mode v) :insert)
          (hexv-cursor v) (hexv-length v) (hexv-modified v)))

;;; --- drawing ----------------------------------------------------------------

(defun %cell-attr (v off cur-pane)
  "Colour for byte OFF in the pane CUR-PANE: the cursor cell in the active pane is
highlighted, an edited-but-unsaved byte is flagged, otherwise normal."
  (let ((cur (= off (hexv-cursor v))))
    (cond ((and cur (eq (hexv-pane v) cur-pane)) (role :focused))
          (cur                                   (role :input-focused))
          ((and (hexv-modified v) (gethash off (hexv-changed v))) (role :error))  ; flagged only while dirty
          (t                                     (role :normal)))))

(defmethod draw ((v hex-view))
  (let* ((b (view-bounds v)) (w (rect-width b)) (h (rect-height b))
         (ax (rect-ax b)) (ay (rect-ay b))
         (n (hexv-length v)) (top (hexv-top v)))
    (dotimes (r h)
      (fill-row v 0 r w (role :normal))
      (let ((base (* (+ top r) +bpr+)))
        (when (< base (max 1 n))                        ; an empty file still shows its offset row
          (draw-text v 0 r (format nil "~8,'0X" base) (role :label))
          (dotimes (i +bpr+)
            (let ((off (+ base i)))
              (when (< off n)
                (let ((byte (aref (hexv-bytes v) off)))
                  (draw-text v (%hex-col i) r (format nil "~2,'0X" byte) (%cell-attr v off :hex))
                  (draw-text v (%ascii-col i) r
                             (string (if (<= 32 byte 126) (code-char byte) #\.))
                             (%cell-attr v off :ascii)))))))))
    ;; a real block cursor in the active pane (only when focused); in insert mode it can
    ;; sit at the append position (offset = length), including an empty buffer.
    (when (and (view-focused-p v) *screen* (or (plusp n) (eq (hexv-mode v) :insert)))
      (let* ((off (hexv-cursor v)) (row (- (floor off +bpr+) top)) (col (mod off +bpr+)))
        (when (<= 0 row (1- h))
          (let ((cx (if (eq (hexv-pane v) :hex)
                        (+ (%hex-col col) (hexv-nibble v))
                        (%ascii-col col))))
            (when (< cx w)
              (set-cursor-pos *screen* (+ ax cx) (+ ay row))
              (set-cursor-shape :block)
              (show-cursor *screen*))))))))

;;; --- input ------------------------------------------------------------------

(defun %edit-key-p (mods)                               ; a plain edit key (Shift ok for A-F / uppercase)
  (not (logtest mods (logior +md-ctrl+ +md-alt+))))

(defun %ctrl-char-p (ks mods ch)
  (and (characterp ks) (char-equal ks ch) (logtest mods +md-ctrl+)))

(defun %hx-save-report (v)
  "Save V and record the outcome (bytes written, or the failure) as its status note."
  (multiple-value-bind (path err) (hex-save v)
    (setf (hexv-message v)
          (cond (path (format nil "saved ~D byte~:P to ~A" (hexv-length v) (file-namestring path)))
                (err  (format nil "save failed: ~A" err))
                (t    "no file — nothing to save")))
    (invalidate v)))

(defmethod handle-event ((v hex-view) (e key-event))
  (let ((ks (event-keysym e)) (mods (event-modifiers e)))
    (cond
      ((eql ks :left)  (%move v -1)                     (setf (handled-p e) t))
      ((eql ks :right) (%move v 1)                      (setf (handled-p e) t))
      ((eql ks :up)    (%move v (- +bpr+))              (setf (handled-p e) t))
      ((eql ks :down)  (%move v +bpr+)                  (setf (handled-p e) t))
      ((eql ks :pgup)  (%move v (- (* +bpr+ (%page v)))) (setf (handled-p e) t))
      ((eql ks :pgdn)  (%move v (* +bpr+ (%page v)))    (setf (handled-p e) t))
      ((eql ks :home)  (%goto v 0)                      (setf (handled-p e) t))
      ((eql ks :end)   (%goto v (%max-cursor v))        (setf (handled-p e) t))
      ((eql ks :ins)   (hex-toggle-mode v)              (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\s) (%hx-save-report v)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\z) (hex-undo v)          (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\y) (hex-redo v)          (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\g) (hex-prompt-goto v)   (setf (handled-p e) t))
      ((%ctrl-char-p ks mods #\f) (hex-prompt-find v)   (setf (handled-p e) t))
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
  (let ((base (* (+ (hexv-top v) (mouse-row v e)) +bpr+)))
    (multiple-value-bind (pane i) (%col->byte (mouse-col v e))
      (when (and pane (< (+ base i) (hexv-length v)))
        (setf (hexv-pane v) pane (hexv-cursor v) (+ base i) (hexv-nibble v) 0)
        (invalidate v))))
  (setf (handled-p e) t))

(defmethod handle-event ((v hex-view) (e wheel-event))
  (scroll-to v (+ (hexv-top v) (* 3 (event-delta e))))
  (setf (handled-p e) t))

;;; widget-intrinsic keys, declared as data for the keybinding reference (#4)
(defmethod view-key-hints ((v hex-view))
  (declare (ignore v))
  '(("Left / Right" . "move one byte")
    ("Up / Down"    . "move one row (16 bytes)")
    ("PgUp / PgDn"  . "page up / down")
    ("Home / End"   . "start / end of file")
    ("Tab"          . "switch the hex / ASCII pane")
    ("Insert"       . "toggle overwrite / insert mode")
    ("0-9 a-f"      . "edit the byte's nibbles (hex pane)")
    ("(printable)"  . "edit the byte (ASCII pane)")
    ("Bksp / Del"   . "delete a byte (insert mode)")
    ("Ctrl+F"       . "find hex bytes or /text (empty = find next)")
    ("Ctrl+G"       . "go to a hex offset")
    ("Ctrl+Z / Ctrl+Y" . "undo / redo")
    ("Ctrl+S"       . "save to the file")))
