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
  (fiveam:is (< (%hex-end) (%ascii-col 0)) "the ASCII gutter is right of the hex pane")
  (fiveam:is (<= (%row-w) 78) "a full row fits an 80-column window interior")
  (fiveam:is (= 4 (- (%hex-col 8) (%hex-col 7))) "a wider gap (3+1) splits the two groups of 8 bytes"))

(fiveam:test col->byte-roundtrips
  (dotimes (i +bpr+)
    (fiveam:is (equal (list :hex i)   (multiple-value-list (%col->byte (%hex-col i)))))
    (fiveam:is (equal (list :ascii i) (multiple-value-list (%col->byte (%ascii-col i))))))
  (fiveam:is-false (%col->byte 0) "a column in the offset field addresses no byte"))

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
    (%move v +bpr+) (fiveam:is (= 32 (hexv-cursor v)) "down one row = +16 bytes")))

(fiveam:test ensure-visible-scrolls
  (let ((v (%view (%range 4000))))                      ; ~250 rows, a 20-row page
    (fiveam:is (= 0 (hexv-top v)))
    (%goto v 3999)                                      ; jump to the end
    (fiveam:is (plusp (hexv-top v)) "scrolls to reveal the cursor")
    (fiveam:is (<= (hexv-top v) (scroll-max v)) "never scrolls past the last page")))

(fiveam:test scroll-protocol
  (let ((v (%view (%range 1000))))                      ; 63 rows
    (fiveam:is (= 20 (scroll-page v)))
    (fiveam:is (= (max 0 (- (%rows v) 20)) (scroll-max v)))
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
