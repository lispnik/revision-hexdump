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
   (modified :initform nil :accessor hexv-modified     ; unsaved edits?
             :documentation "True once a byte has been edited since the last load/save.")
   (changed  :initform (make-hash-table) :accessor hexv-changed)  ; offset -> T, edited since load/save
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
(defun %rows (v) (max 1 (ceiling (max 1 (hexv-length v)) +bpr+)))

;;; --- load / save ------------------------------------------------------------

(defun hex-load (v path)
  "Load PATH's bytes into V, resetting the cursor and edit state."
  (setf (hexv-bytes v) (read-file-bytes path)
        (hexv-filename v) path
        (hexv-cursor v) 0 (hexv-top v) 0 (hexv-nibble v) 0
        (hexv-modified v) nil)
  (clrhash (hexv-changed v))
  (invalidate v)
  v)

(defun hex-save (v &optional (path (hexv-filename v)))
  "Write V's buffer to PATH (default its own filename); clear the modified state.
Returns the path on success, NIL when there is no path."
  (when path
    (write-file-bytes path (hexv-bytes v))
    (setf (hexv-filename v) path (hexv-modified v) nil)
    (clrhash (hexv-changed v))
    (invalidate v)
    path))

;;; --- movement ---------------------------------------------------------------

(defun %ensure-visible (v)
  "Scroll so the cursor's row is on screen."
  (let ((row (floor (hexv-cursor v) +bpr+)) (page (%page v)))
    (cond ((< row (hexv-top v)) (setf (hexv-top v) row))
          ((>= row (+ (hexv-top v) page)) (setf (hexv-top v) (1+ (- row page)))))))

(defun %goto (v off)
  "Move the cursor to byte OFF (clamped), reset the nibble, and reveal it."
  (let ((n (hexv-length v)))
    (when (plusp n)
      (setf (hexv-cursor v) (max 0 (min off (1- n)))
            (hexv-nibble v) 0)
      (%ensure-visible v))))

(defun %move (v delta) (%goto v (+ (hexv-cursor v) delta)))

;;; --- editing (overwrite; preserves the buffer size) -------------------------

(defun %set-byte (v off value)
  "Overwrite byte OFF with VALUE, recording the edit."
  (setf (aref (hexv-bytes v) off) (logand value #xff)
        (gethash off (hexv-changed v)) t
        (hexv-modified v) t)
  (when (hexv-on-change v) (funcall (hexv-on-change v) v))
  (invalidate v))

(defun %hex-input (v digit)
  "Apply hex DIGIT (0-15) to the current byte's active nibble; the low nibble
finishes the byte and advances the cursor."
  (when (plusp (hexv-length v))
    (let* ((off (hexv-cursor v)) (b (aref (hexv-bytes v) off)))
      (if (zerop (hexv-nibble v))
          (progn (%set-byte v off (logior (ash digit 4) (logand b #x0f)))
                 (setf (hexv-nibble v) 1))
          (progn (%set-byte v off (logior (logand b #xf0) digit))
                 (%move v 1))))))                       ; %move resets the nibble to hi

(defun %ascii-input (v char)
  "Overwrite the current byte with CHAR's code and advance the cursor."
  (when (plusp (hexv-length v))
    (%set-byte v (hexv-cursor v) (char-code char))
    (%move v 1)))

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
  (format nil " ~(~a~) 0x~X/0x~X~:[~; *~] "
          (hexv-pane v) (hexv-cursor v) (hexv-length v) (hexv-modified v)))

;;; --- drawing ----------------------------------------------------------------

(defun %cell-attr (v off cur-pane)
  "Colour for byte OFF in the pane CUR-PANE: the cursor cell in the active pane is
highlighted, an edited-but-unsaved byte is flagged, otherwise normal."
  (let ((cur (= off (hexv-cursor v))))
    (cond ((and cur (eq (hexv-pane v) cur-pane)) (role :focused))
          (cur                                   (role :input-focused))
          ((gethash off (hexv-changed v))        (role :error))
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
    ;; a real block cursor in the active pane (only when focused)
    (when (and (view-focused-p v) *screen* (plusp n))
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
      ((eql ks :end)   (%goto v (1- (hexv-length v)))   (setf (handled-p e) t))
      ((and (characterp ks) (char-equal ks #\s) (logtest mods +md-ctrl+))
       (hex-save v)                                     (setf (handled-p e) t))
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
    ("0-9 a-f"      . "overwrite the byte's nibbles (hex pane)")
    ("(printable)"  . "overwrite the byte (ASCII pane)")
    ("Ctrl+S"       . "save to the file")))
