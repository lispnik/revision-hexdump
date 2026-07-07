;;;; tests.lisp --- the revision-hexdump test suite (FiveAM).
;;;;
;;;; Run with:  sbcl --script tests/tests.lisp   (or `make test').
;;;; setup.lisp puts the sibling `revision' checkout — and its vendored FiveAM — on
;;;; the ASDF registry.  The suite is pure logic over the widget (column geometry,
;;;; buffer load/edit/save, movement, scrolling, the window wiring); it needs no
;;;; terminal, so it runs headless in CI.

(require :asdf)
(load (merge-pathnames "../setup.lisp" (or *load-truename* *load-pathname*)))
(handler-bind ((warning #'muffle-warning)
               #+sbcl (sb-ext:compiler-note #'muffle-warning))
  (asdf:load-system :fiveam)
  (asdf:load-system :revision-hexdump))

(in-package #:revision-hexdump)

(fiveam:def-suite hexdump :description "revision-hexdump: an editable hex editor widget.")
(fiveam:in-suite hexdump)

;;; --- helpers ----------------------------------------------------------------

(defun %buf (seq)
  "An (unsigned-byte 8) buffer holding SEQ (a sequence of small integers)."
  (let* ((n (length seq)) (b (make-array n :element-type '(unsigned-byte 8) :adjustable t :fill-pointer n)))
    (replace b (map '(vector (unsigned-byte 8)) (lambda (x) (logand x #xff)) seq))
    b))

(defun %view (&optional (seq #()))
  "A hex-view holding SEQ, laid out to a screen-less 78x20 rectangle."
  (let ((v (make-instance 'hex-view :bytes (%buf seq))))
    (layout v (rect 0 0 78 20))
    v))

(defun %range (n) (%buf (loop for i below n collect (mod i 256))))

(defmacro with-temp-file ((path init) &body body)
  "Bind PATH to a fresh temp file initialised with INIT (a byte sequence)."
  `(uiop:with-temporary-file (:pathname ,path :type "bin")
     (write-file-bytes ,path ,init)
     ,@body))

;;; --- column geometry --------------------------------------------------------

(fiveam:test geometry
  (fiveam:is (< (%hex-col 0) (%hex-col 15)) "hex columns increase left-to-right")
  (fiveam:is (< (%hex-end 16) (%ascii-col 16 0)) "the ASCII gutter is right of the hex pane")
  (fiveam:is (<= (%row-w 16) 78) "a full 16-byte row fits an 80-column window interior")
  (fiveam:is (= 4 (- (%hex-col 8) (%hex-col 7))) "a wider gap (3+1) splits the groups of 8 bytes"))

(fiveam:test col->byte-roundtrips
  (dotimes (i 16)
    (fiveam:is (equal (list :hex i)   (multiple-value-list (%col->byte 16 (%hex-col i)))))
    (fiveam:is (equal (list :ascii i) (multiple-value-list (%col->byte 16 (%ascii-col 16 i))))))
  (fiveam:is-false (%col->byte 16 0) "a column in the offset field addresses no byte"))

(fiveam:test adaptive-width
  (fiveam:is (= 16 (%fit-bpr 78)) "16 bytes/row fits an 80-column window interior")
  (fiveam:is (= 8  (%fit-bpr 40)) "a narrow window drops to 8 bytes/row")
  (fiveam:is (<= 32 (%fit-bpr 200)) "a wide window shows more per row")
  (let ((v (%view (%range 100))))
    (layout v (rect 0 0 140 20))
    (fiveam:is (= (%fit-bpr 140) (hexv-bpr v)) "layout sizes bytes-per-row to the view width")))

;;; --- file I/O ---------------------------------------------------------------

(fiveam:test file-roundtrip
  (with-temp-file (p (%buf '(0 1 2 255)))
    (let ((b (read-file-bytes p)))
      (fiveam:is (= 4 (length b)))
      (fiveam:is (equalp #(0 1 2 255) (coerce b 'vector))))))

(fiveam:test load
  (with-temp-file (p (%buf '(#xDE #xAD #xBE #xEF)))
    (let ((v (%view)))
      (hex-load v p)
      (fiveam:is (= 4 (hexv-length v)))
      (fiveam:is (= 0 (hexv-cursor v)))
      (fiveam:is-false (hexv-modified v))
      (fiveam:is (= #xDE (aref (hexv-bytes v) 0))))))

;;; --- editing ----------------------------------------------------------------

(fiveam:test hex-nibble-edit
  (let ((v (%view #(#x00 #x00))))
    (%hex-input v 10)                                   ; 'a' -> high nibble
    (fiveam:is (= #xA0 (aref (hexv-bytes v) 0)))
    (fiveam:is (= 1 (hexv-nibble v)) "nibble advances to low")
    (fiveam:is (= 0 (hexv-cursor v)) "cursor stays on the byte")
    (%hex-input v 5)                                    ; '5' -> low nibble, finishes the byte
    (fiveam:is (= #xA5 (aref (hexv-bytes v) 0)))
    (fiveam:is (= 1 (hexv-cursor v)) "cursor advances after the low nibble")
    (fiveam:is (= 0 (hexv-nibble v)) "nibble resets to high")
    (fiveam:is-true (hexv-modified v))
    (fiveam:is-true (gethash 0 (hexv-changed v)))))

(fiveam:test ascii-edit
  (let ((v (%view #(0 0 0))))
    (setf (hexv-pane v) :ascii)
    (%ascii-input v #\H) (%ascii-input v #\i)
    (fiveam:is (= (char-code #\H) (aref (hexv-bytes v) 0)))
    (fiveam:is (= (char-code #\i) (aref (hexv-bytes v) 1)))
    (fiveam:is (= 2 (hexv-cursor v)))
    (fiveam:is-true (hexv-modified v))))

(fiveam:test editing-preserves-size
  (let ((v (%view (%range 8))))
    (%hex-input v 15) (%hex-input v 15)                 ; overwrite byte 0 with 0xFF
    (fiveam:is (= 8 (hexv-length v)) "overwrite editing keeps the buffer size")
    (fiveam:is (= #xFF (aref (hexv-bytes v) 0)))))

;;; --- movement + scrolling ---------------------------------------------------

(fiveam:test movement
  (let ((v (%view (%range 40))))
    (%move v 5)    (fiveam:is (= 5 (hexv-cursor v)))
    (%move v -100) (fiveam:is (= 0 (hexv-cursor v)) "clamped at the start")
    (%move v 1000) (fiveam:is (= 39 (hexv-cursor v)) "clamped at the end")
    (%goto v 16)   (fiveam:is (= 16 (hexv-cursor v)))
    (%move v (hexv-bpr v)) (fiveam:is (= 32 (hexv-cursor v)) "down one row = +bpr bytes")))

(fiveam:test ensure-visible-scrolls
  (let ((v (%view (%range 4000))))                      ; ~250 rows, a 20-row page
    (fiveam:is (= 0 (hexv-top v)))
    (%goto v 3999)                                      ; jump to the end
    (fiveam:is (plusp (hexv-top v)) "scrolls to reveal the cursor")
    (fiveam:is (<= (hexv-top v) (scroll-max v)) "never scrolls past the last page")))

(fiveam:test scroll-protocol
  (let ((v (%view (%range 1000))))                      ; 63 rows
    (fiveam:is (= (%page v) (scroll-page v)) "scroll-page is the dump height (minus ruler + inspector)")
    (fiveam:is (< (%page v) 20) "the ruler and inspector take rows off the 20-row view")
    (fiveam:is (= (max 0 (- (%rows v) (%page v))) (scroll-max v)) "scroll-max = rows - page")
    (scroll-to v 999) (fiveam:is (= (scroll-max v) (scroll-pos v)) "scroll-to clamps to max")
    (scroll-to v -5)  (fiveam:is (= 0 (scroll-pos v)) "scroll-to clamps to 0")))

(fiveam:test toggle-pane
  (let ((v (%view #(1 2 3))))
    (fiveam:is (eq :hex (hexv-pane v)))
    (hex-toggle-pane v) (fiveam:is (eq :ascii (hexv-pane v)))
    (hex-toggle-pane v) (fiveam:is (eq :hex (hexv-pane v)))))

;;; --- save round-trip --------------------------------------------------------

(fiveam:test save-roundtrip
  (with-temp-file (p (%buf '(0 0 0 0)))
    (let ((v (%view)))
      (hex-load v p)
      (%hex-input v 15) (%hex-input v 15)               ; byte 0 = 0xFF, cursor -> 1
      (setf (hexv-pane v) :ascii)
      (%ascii-input v #\Z)                              ; byte 1 = 'Z'
      (fiveam:is-true (hexv-modified v))
      (hex-save v)
      (fiveam:is-false (hexv-modified v) "saving clears the modified flag")
      (let ((b (read-file-bytes p)))
        (fiveam:is (= #xFF (aref b 0)))
        (fiveam:is (= (char-code #\Z) (aref b 1)))))))

;;; --- new file / save-as -----------------------------------------------------

(fiveam:test new-buffer-is-insert-mode
  (multiple-value-bind (win focus) (make-hexdump)   ; no path -> empty buffer
    (declare (ignore win))
    (fiveam:is (zerop (hexv-length focus)) "a new buffer is empty")
    (fiveam:is (eq :insert (hexv-mode focus)) "and opens in insert mode, ready to type")))

(fiveam:test opened-file-stays-overwrite
  (with-temp-file (p (%buf '(1 2 3)))
    (multiple-value-bind (win focus) (make-hexdump p)
      (declare (ignore win))
      (fiveam:is (eq :overwrite (hexv-mode focus)) "opening an existing file stays in overwrite mode"))))

(fiveam:test author-and-save-new-file
  (with-temp-file (p (%buf #()))                    ; an empty destination path
    (let ((v (%view)))
      (setf (hexv-mode v) :insert)
      (%ascii-input v #\N) (%ascii-input v #\O)      ; author two bytes
      (hex-save v p)                                 ; save-as (explicit path) reuses hex-save
      (fiveam:is (equalp #(78 79) (coerce (read-file-bytes p) 'vector)) "authored bytes are written to the chosen path")
      (fiveam:is-false (hexv-modified v) "saving a new file marks it clean"))))

;;; --- widget / window integration --------------------------------------------

(fiveam:test key-hints
  (fiveam:is (assoc "Tab" (view-key-hints (%view)) :test #'string=)
             "view-key-hints declares the pane-toggle key (for the reference)"))

(fiveam:test draw-headless
  (let ((v (%view (%range 100))) (revision:*screen* nil))
    (fiveam:finishes (draw v))))                        ; draws nothing without a screen, but must not error

(fiveam:test window-integration
  (with-temp-file (p (%buf '(1 2 3 4)))
    (multiple-value-bind (win focus open) (make-hexdump p)
      (declare (ignore open))
      (layout win (rect 0 0 80 24))
      (fiveam:is (typep win 'hex-window))
      (fiveam:is (typep focus 'hex-view))
      (fiveam:is (eq focus (window-scroll-target win)) "the hex-view drives the frame scrollbar")
      (fiveam:is-false (window-esc-dismissable-p win) "a hex window isn't Esc-dismissable (unsaved work)")
      (fiveam:is-false (window-dirty-p win) "clean right after open")
      (let ((v (%hx-view win)))
        (%hex-input v 1) (%hex-input v 2)               ; edit byte 0
        (fiveam:is-true (window-dirty-p win) "editing marks the window dirty")
        (fiveam:is (equal (list (namestring p)) (window-save-state win))
                   "window-save-state remembers the file")))))

;;; --- undo / redo + the clean checkpoint -------------------------------------

(fiveam:test noop-edit-does-nothing
  (let ((v (%view #(#x42))))
    (%set-byte v 0 #x42)                                ; writing a byte its current value
    (fiveam:is-false (hexv-modified v) "a no-op write does not dirty the buffer")
    (fiveam:is (zerop (hexv-hpos v)) "and records no history")))

(fiveam:test undo-redo
  (let ((v (%view #(#x11 #x22 #x33))))
    (%hex-input v 10) (%hex-input v 10)                 ; byte 0 -> 0xAA (two nibble edits)
    (fiveam:is (= #xAA (aref (hexv-bytes v) 0)))
    (hex-undo v) (fiveam:is (= #xA1 (aref (hexv-bytes v) 0)) "undo reverts the low nibble (0xAA -> 0xA1)")
    (hex-undo v) (fiveam:is (= #x11 (aref (hexv-bytes v) 0)) "undo reverts to the original byte")
    (fiveam:is-false (hexv-modified v) "back at the load checkpoint -> clean")
    (hex-redo v) (hex-redo v)
    (fiveam:is (= #xAA (aref (hexv-bytes v) 0)) "redo reapplies both nibbles")
    (fiveam:is-true (hexv-modified v))))

(fiveam:test undo-redo-empty
  (let ((v (%view #(1 2))))
    (fiveam:is-false (hex-undo v) "nothing to undo on a fresh buffer")
    (fiveam:is-false (hex-redo v) "nothing to redo")))

(fiveam:test new-edit-truncates-redo
  (let ((v (%view #(0 0 0))))
    (%set-byte v 0 #xAA) (%set-byte v 1 #xBB)
    (hex-undo v)                                        ; redo tail now holds the byte-1 edit
    (%set-byte v 2 #xCC)                                ; a fresh edit discards it
    (fiveam:is-false (hex-redo v) "a fresh edit truncates the redo tail")
    (fiveam:is (= #xAA (aref (hexv-bytes v) 0)))
    (fiveam:is (= #x00 (aref (hexv-bytes v) 1)) "the undone edit stays undone")
    (fiveam:is (= #xCC (aref (hexv-bytes v) 2)))))

(fiveam:test modified-checkpoint
  (with-temp-file (p (%buf '(0 0)))
    (let ((v (%view)))
      (hex-load v p)
      (fiveam:is-false (hexv-modified v))
      (%set-byte v 0 #x99) (fiveam:is-true (hexv-modified v))
      (hex-save v)         (fiveam:is-false (hexv-modified v) "clean right after save")
      (hex-undo v)         (fiveam:is-true (hexv-modified v) "undoing past the saved state is dirty again")
      (hex-redo v)         (fiveam:is-false (hexv-modified v) "redoing back to the saved state is clean again"))))

;;; --- robust save ------------------------------------------------------------

(fiveam:test save-error-is-caught
  (let ((v (%view #(1 2 3))))
    (%set-byte v 0 #xFF)
    (multiple-value-bind (path err) (hex-save v "/no-such-dir-xqz/nope.bin")
      (fiveam:is-false path "a failed save returns no path")
      (fiveam:is-true err "a failed save returns the error rather than signalling")
      (fiveam:is-true (hexv-modified v) "a failed save leaves the buffer modified"))))

;;; --- insert / delete mode ---------------------------------------------------

(fiveam:test insert-grows-buffer
  (let ((v (%view #(#xAA #xBB))))
    (setf (hexv-mode v) :insert (hexv-cursor v) 1)
    (%insert-byte v 1 #xCC)                             ; insert between AA and BB
    (fiveam:is (= 3 (hexv-length v)) "insert grows the buffer")
    (fiveam:is (equalp #(#xAA #xCC #xBB) (coerce (hexv-bytes v) 'vector)))))

(fiveam:test insert-into-empty
  (let ((v (%view #())))
    (setf (hexv-mode v) :insert)
    (%ascii-input v #\H) (%ascii-input v #\i)           ; type into an empty buffer
    (fiveam:is (= 2 (hexv-length v)) "insert mode can edit an empty file")
    (fiveam:is (string= "Hi" (map 'string #'code-char (hexv-bytes v))))
    (fiveam:is (= 2 (hexv-cursor v)) "cursor sits at the append position")))

(fiveam:test delete-shrinks-buffer
  (let ((v (%view #(#x11 #x22 #x33))))
    (setf (hexv-mode v) :insert (hexv-cursor v) 1)
    (%delete-byte v 1)                                  ; remove 0x22
    (fiveam:is (= 2 (hexv-length v)))
    (fiveam:is (equalp #(#x11 #x33) (coerce (hexv-bytes v) 'vector)))))

(fiveam:test insert-delete-undo
  (let ((v (%view #(#x11 #x22))))
    (setf (hexv-mode v) :insert (hexv-cursor v) 1)
    (%insert-byte v 1 #x99)                             ; #(11 99 22)
    (%delete-byte v 0)                                  ; #(99 22)
    (fiveam:is (equalp #(#x99 #x22) (coerce (hexv-bytes v) 'vector)))
    (hex-undo v)                                        ; undo delete -> #(11 99 22)
    (fiveam:is (equalp #(#x11 #x99 #x22) (coerce (hexv-bytes v) 'vector)) "undo restores a deleted byte")
    (hex-undo v)                                        ; undo insert -> #(11 22)
    (fiveam:is (equalp #(#x11 #x22) (coerce (hexv-bytes v) 'vector)) "undo removes an inserted byte")
    (fiveam:is-false (hexv-modified v) "back at the load checkpoint")
    (hex-redo v) (hex-redo v)                           ; redo both
    (fiveam:is (equalp #(#x99 #x22) (coerce (hexv-bytes v) 'vector)) "redo reapplies insert then delete")))

(fiveam:test mode-toggle-clamps-cursor
  (let ((v (%view #(1 2 3))))
    (setf (hexv-mode v) :insert (hexv-cursor v) 3)      ; the append position
    (hex-toggle-mode v)                                 ; back to overwrite
    (fiveam:is (eq :overwrite (hexv-mode v)))
    (fiveam:is (= 2 (hexv-cursor v)) "leaving insert mode clamps the append cursor onto a byte")))

;;; --- search -----------------------------------------------------------------

(fiveam:test parse-search
  (fiveam:is (equalp #(#xDE #xAD #xBE #xEF) (%parse-search "deadbeef")))
  (fiveam:is (equalp #(#xDE #xAD) (%parse-search "de ad")))
  (fiveam:is (equalp #(72 105) (%parse-search "/Hi")) "a leading / means literal ASCII")
  (fiveam:is-false (%parse-search "xyz") "non-hex is rejected")
  (fiveam:is-false (%parse-search "abc") "an odd number of hex digits is rejected"))

(fiveam:test search
  (let ((v (%view (map 'list #'char-code "the fox and the dog"))))
    (fiveam:is (= 4 (hex-search v (%parse-search "/fox") 0)) "finds a literal string")
    (fiveam:is (= 12 (hex-search v (%parse-search "/the"))) "find-next continues past the cursor")
    (fiveam:is (= 0 (hex-search v (%parse-search "/the"))) "wraps around to the first match")
    (fiveam:is-false (hex-search v (%parse-search "/zzz") 0) "reports no match")))

;;; --- selection + clipboard --------------------------------------------------

(fiveam:test selection-range
  (let ((v (%view (%range 10))))
    (fiveam:is-false (hexv-selection v) "no selection by default")
    (%sel-anchor v t) (%move v 3)                       ; Shift-move from 0 to 3
    (fiveam:is (equal '(0 . 3) (hexv-selection v)) "shift-move sets an inclusive range")
    (%sel-anchor v nil)                                 ; a plain move collapses it
    (fiveam:is-false (hexv-selection v))))

(fiveam:test copy-paste-overwrite
  (let ((v (%view #(#x11 #x22 #x33 #x44 #x55))))
    (%sel-anchor v t) (%move v 1)                       ; select bytes 0..1 (11 22)
    (hex-copy v)
    (fiveam:is (equalp #(#x11 #x22) (coerce *clipboard* 'vector)) "copy grabs the selection")
    (%goto v 3)                                         ; overwrite mode: paste over bytes 3..4
    (hex-paste v)
    (fiveam:is (equalp #(#x11 #x22 #x33 #x11 #x22) (coerce (hexv-bytes v) 'vector))
               "overwrite paste replaces in place, keeping the size")))

(fiveam:test cut-and-paste-insert
  (let ((v (%view #(#xAA #xBB #xCC #xDD))))
    (setf (hexv-mode v) :insert)
    (%goto v 1) (%sel-anchor v t) (%move v 1)           ; select bytes 1..2 (BB CC)
    (hex-cut v)
    (fiveam:is (equalp #(#xAA #xDD) (coerce (hexv-bytes v) 'vector)) "cut removes the selection")
    (fiveam:is (equalp #(#xBB #xCC) (coerce *clipboard* 'vector)) "and keeps it on the clipboard")
    (%goto v 2) (hex-paste v)                           ; insert-paste at the end
    (fiveam:is (equalp #(#xAA #xDD #xBB #xCC) (coerce (hexv-bytes v) 'vector)) "insert paste grows the buffer")))

(fiveam:test cut-paste-is-one-undo
  (let ((v (%view #(1 2 3 4 5))))
    (setf (hexv-mode v) :insert)
    (%goto v 1) (%sel-anchor v t) (%move v 2)           ; select 3 bytes (1..3)
    (hex-cut v)                                         ; delete 3 bytes -> one undo step
    (fiveam:is (= 2 (hexv-length v)))
    (hex-undo v)
    (fiveam:is (equalp #(1 2 3 4 5) (coerce (hexv-bytes v) 'vector)) "one undo restores the whole cut")
    (fiveam:is-false (hexv-modified v) "and returns to the clean checkpoint")))

;;; --- replace ----------------------------------------------------------------

(fiveam:test replace-same-length
  (let ((v (%view (map 'list #'char-code "a-b-c-d"))))
    (fiveam:is (= 3 (hex-replace-all v (%parse-search "/-") (%parse-search "/+"))) "replaces all, returns the count")
    (fiveam:is (string= "a+b+c+d" (map 'string #'code-char (hexv-bytes v))))))

(fiveam:test replace-different-length-and-undo
  (let ((v (%view (map 'list #'char-code "xAAx"))))
    (hex-replace-all v (%parse-search "/AA") (%parse-search "/BBB"))   ; grow: 2 -> 3 bytes
    (fiveam:is (string= "xBBBx" (map 'string #'code-char (hexv-bytes v))) "replacement may change length")
    (hex-undo v)
    (fiveam:is (string= "xAAx" (map 'string #'code-char (hexv-bytes v))) "one undo reverts the whole replace-all")))

(fiveam:test replace-delete
  (let ((v (%view (map 'list #'char-code "a,b,c"))))
    (hex-replace-all v (%parse-search "/,") (make-array 0 :element-type '(unsigned-byte 8)))  ; empty = delete
    (fiveam:is (string= "abc" (map 'string #'code-char (hexv-bytes v))) "an empty replacement deletes the matches")))

;;; --- data inspector ---------------------------------------------------------

(fiveam:test data-inspector
  (let ((v (%view #(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08))))
    (flet ((g (k) (cdr (assoc k (hexv-inspect v) :test #'string=))))
      (fiveam:is (string= "1" (g "u8")))
      (fiveam:is (string= "513" (g "u16")) "0x0201 little-endian = 513")
      (fiveam:is (string= "67305985" (g "u32")) "0x04030201 little-endian")
      (hex-toggle-endian v)                             ; -> big-endian
      (fiveam:is (string= "258" (g "u16")) "0x0102 big-endian = 258")
      (fiveam:is (string= "16909060" (g "u32")) "0x01020304 big-endian"))))

(fiveam:test inspector-signed-and-short
  (let ((v (%view #(#xFF))))
    (flet ((g (k) (cdr (assoc k (hexv-inspect v) :test #'string=))))
      (fiveam:is (string= "255" (g "u8")))
      (fiveam:is (string= "-1"  (g "i8")) "0xFF as signed = -1")
      (fiveam:is (string= "—"   (g "u16")) "a type needing more bytes than remain shows —"))))

(fiveam:test inspector-floats
  (let ((v (%view #(#x00 #x00 #x80 #x3F))))            ; IEEE-754 single 1.0, little-endian
    (fiveam:is (= 1.0f0 (%read-f32 v 0 nil)) "decodes a float32"))
  (let ((v (%view #(#x00 #x00 #x00 #x00 #x00 #x00 #xF0 #x3F))))  ; double 1.0, little-endian
    (fiveam:is (= 1.0d0 (%read-f64 v 0 nil)) "decodes a float64")))

;;; --- large-file (paged, read-only) source -----------------------------------

(fiveam:test large-file-editable
  (with-temp-file (p (%buf (loop for i below 20 collect i)))
    (let ((v (%view)) (*max-in-memory* 4) (*fs-page-size* 8))   ; force the paged path, tiny pages
      (hex-load v p)
      (fiveam:is-false (hexv-readonly v) "a large paged file is editable")
      (fiveam:is-true (hexv-resizable-p v) "and resizable via the piece table")
      (fiveam:is (= 20 (hexv-length v)) "its length is known without loading it")
      (fiveam:is (= 19 (%bref v 19)) "a paged read across page boundaries")
      (%set-byte v 3 #xFF)                                ; overwrite
      (fiveam:is (= #xFF (%bref v 3)) "overwrite through the piece table")
      (%insert-byte v 0 #x99)                             ; insert grows it
      (fiveam:is (= 21 (hexv-length v)) "insert grows a paged file")
      (fiveam:is (= #x99 (%bref v 0)) "the inserted byte")
      (fiveam:is (= 0 (%bref v 1)) "and the original bytes shifted right")
      (fiveam:is (= #xFF (%bref v 4)) "the overwritten byte shifted too")
      (%delete-byte v 0)                                  ; delete shrinks it back
      (fiveam:is (= 20 (hexv-length v)))
      (fiveam:is (= #xFF (%bref v 3)) "and the overwrite is back at offset 3")
      (hex-undo v) (hex-undo v) (hex-undo v)              ; undo delete, insert, overwrite
      (fiveam:is (= 3 (%bref v 3)) "undo reverts the whole session")
      (fiveam:is (= 20 (hexv-length v)))
      (fiveam:is-false (hexv-modified v) "back to the clean checkpoint")
      (fiveam:is (= 10 (hex-search v (%buf '(10 11 12)) 0)) "search scans the piece table")
      (%close-source v)
      (fiveam:is-false (hexv-source v) "close releases the source"))))

(fiveam:test large-file-save
  (with-temp-file (p (%buf (loop for i below 30 collect i)))
    (let ((v (%view)) (*max-in-memory* 4) (*fs-page-size* 8))
      (hex-load v p)
      (%set-byte v 0 #xAA)                                ; overwrite byte 0
      (%insert-byte v 30 #xCC)                            ; append a byte (size grows)
      (%delete-byte v 5)                                  ; delete byte 5 (size shrinks back)
      (hex-save v)                                        ; streamed to disk, piece by piece
      (fiveam:is-false (hexv-modified v) "streamed save -> clean")
      (let ((bytes (read-file-bytes p)))
        (fiveam:is (= 30 (length bytes)) "one insert + one delete -> net same size")
        (fiveam:is (= #xAA (aref bytes 0)) "overwrite written")
        (fiveam:is (= 4 (aref bytes 4)) "byte before the deletion preserved")
        (fiveam:is (= 6 (aref bytes 5)) "byte 5 deleted; value 6 shifted in")
        (fiveam:is (= #xCC (aref bytes 29)) "appended byte streamed at the end"))
      ;; the reopened source now presents the saved file as one piece
      (fiveam:is (= #xAA (%bref v 0)) "editing continues after save")
      (%close-source v))))

;;; --- inspector char + binary, and toggles -----------------------------------

(fiveam:test inspector-char-and-binary
  (let ((v (%view #(#x6C))))                            ; 'l'
    (flet ((g (k) (cdr (assoc k (hexv-inspect v) :test #'string=))))
      (fiveam:is (string= "l" (g "char")) "printable byte shows as its character")
      (fiveam:is (string= "01101100" (g "bin")) "and as 8-bit binary")))
  (let ((v (%view #(#x00))))                            ; NUL is not printable
    (fiveam:is (string= "." (cdr (assoc "char" (hexv-inspect v) :test #'string=))) "a control byte's char is a dot")))

(fiveam:test inspector-toggle-changes-page
  (let ((v (%view (%range 100))))
    (let ((with (%page v)))
      (hex-toggle-inspector v)
      (fiveam:is (= (+ with 2) (%page v)) "hiding the inspector gives its two rows back to the dump")
      (fiveam:is-false (hexv-inspector v)))))

(fiveam:test control-glyphs
  (let ((v (%view #(0 9 65 127 200))))
    (fiveam:is (char= #\A (%ascii-glyph v 65)) "printable is itself")
    (fiveam:is (char= (code-char #x2400) (%ascii-glyph v 0)) "NUL -> ␀ control picture")
    (fiveam:is (char= (code-char #x2421) (%ascii-glyph v 127)) "DEL -> ␡")
    (setf (hexv-ctrl-glyphs v) nil)
    (fiveam:is (char= #\. (%ascii-glyph v 0)) "with glyphs off, control bytes are '.'")))

(fiveam:test offset-base
  (let ((v (%view (%range 20))))
    (fiveam:is (string= "0000000F" (%fmt-offset v 15)) "hex offset by default (15 -> 0F)")
    (setf (hexv-offset-decimal v) t)
    (fiveam:is (string= "00000021" (%fmt-offset v 21)) "decimal when toggled (21 -> 21)")))

(fiveam:test read-only-lock
  (let ((v (%view #(1 2 3))))
    (fiveam:is-false (hexv-readonly v))
    (hex-toggle-lock v)
    (fiveam:is-true (hexv-readonly v) "the lock makes an editable buffer read-only")
    (%set-byte v 0 #xFF)
    (fiveam:is (= 1 (%bref v 0)) "and refuses edits")
    (hex-toggle-lock v)
    (fiveam:is-false (hexv-readonly v) "unlocking restores editing")))

;;; --- bookmarks --------------------------------------------------------------

(fiveam:test bookmarks
  (let ((v (%view (%range 100))))
    (%goto v 10) (hex-toggle-mark v)
    (%goto v 30) (hex-toggle-mark v)
    (%goto v 50) (hex-toggle-mark v)
    (fiveam:is (= 3 (hash-table-count (hexv-marks v))))
    (%goto v 0)
    (hex-next-mark v 1)  (fiveam:is (= 10 (hexv-cursor v)) "next mark after 0 is 10")
    (hex-next-mark v 1)  (fiveam:is (= 30 (hexv-cursor v)))
    (hex-next-mark v -1) (fiveam:is (= 10 (hexv-cursor v)) "prev mark from 30 is 10")
    (hex-next-mark v -1) (fiveam:is (= 50 (hexv-cursor v)) "prev wraps to the last mark")
    (%goto v 30) (hex-toggle-mark v)                     ; clear the mark at 30
    (fiveam:is (= 2 (hash-table-count (hexv-marks v))) "toggling clears a mark")))

;;; --- go-to-offset parsing ---------------------------------------------------

(fiveam:test parse-offset
  (fiveam:is (= 31  (%parse-offset "1F")))
  (fiveam:is (= 31  (%parse-offset "0x1F")))
  (fiveam:is (= 255 (%parse-offset "  ff  ")))
  (fiveam:is-false (%parse-offset "xyz"))
  (fiveam:is-false (%parse-offset "")))

;;; --- run --------------------------------------------------------------------

(let ((ok (fiveam:run! 'hexdump)))
  (sb-ext:exit :code (if ok 0 1)))
