;;;; app.lisp --- the hex-editor window, entry points, and desktop registration.
;;;;
;;;; HEX-WINDOW hosts a HEX-VIEW plus a status line.  MAKE-HEXDUMP follows the
;;;; framework's builder contract -- (values WINDOW FOCUS OPEN) -- so the window
;;;; runs full-screen (RUN-HEXDUMP) or inside the desktop (registered as :hexdump).
;;;; The window carries unsaved-edit state: Esc won't silently discard it, closing
;;;; a modified buffer prompts to save, and the open file is remembered across
;;;; sessions via WINDOW-SAVE-STATE.

(in-package #:revision-hexdump)

(defclass hex-window (window) ()
  (:metaclass reactive-class)
  (:documentation "A window hosting a HEX-VIEW and a status line: its SCROLL-TARGET is the hex-view,
Tab toggles the hex/ASCII pane, and it reports unsaved edits so the desktop guards its close."))

(defun %hx-view (w) (find-view w 'hex))

(defun %hx-title (w)
  (let ((v (%hx-view w)))
    (when v
      (setf (window-title w)
            (format nil " ~a~:[~; *~] "
                    (if (hexv-filename v) (file-namestring (hexv-filename v)) "(no file)")
                    (hexv-modified v))))))

(defun %hx-status (w)
  (let ((v (%hx-view w)) (s (find-view w 'status)))
    (when (and v s)
      (let ((sel (hexv-selection v)))
        (setf (static-text-text s)
              (or (and (hexv-message v) (format nil " ~A " (hexv-message v)))   ; transient note takes priority
                  (and sel (format nil " ~D bytes selected · Ctrl-C: copy · Ctrl-X: cut · Ctrl-V: paste · Esc: close "
                                   (1+ (- (cdr sel) (car sel)))))
                  (format nil " ~:[HEX~;ASCII~]/~A · 0x~X of 0x~X · Ins: mode · Ctrl-F: find · Ctrl-R: replace · Ctrl-G: goto · Ctrl-Z: undo · Ctrl-S: save "
                          (eq (hexv-pane v) :ascii)
                          (cond ((hexv-readonly v) "RO") ((eq (hexv-mode v) :insert) "INS") (t "OVR"))
                          (hexv-cursor v) (hexv-length v))))))))

(defmethod draw :before ((w hex-window))    ; keep the title / status live each repaint
  (%hx-status w) (%hx-title w))              ; the data inspector is drawn inside the hex-view

;; Unsaved edits: Esc must not silently discard the buffer, and closing a modified
;; one prompts to save (the desktop's %DT-REQUEST-CLOSE consults these).
(defmethod window-dirty-p ((w hex-window))
  (let ((v (%hx-view w))) (and v (hexv-modified v))))
(defmethod window-esc-dismissable-p ((w hex-window)) (declare (ignore w)) nil)

;; The container consumes Tab (focus movement) before it reaches the focused child,
;; so intercept it here to toggle the pane instead.
(defmethod handle-event ((w hex-window) (e key-event))
  (let ((v (%hx-view w)))
    (if (and v (eql (event-keysym e) :tab))
        (progn (hex-toggle-pane v) (setf (handled-p e) t))
        (call-next-method))))

;; Session persistence: remember (and reopen) the file across restarts.
(defmethod window-save-state ((w hex-window))
  (let ((v (%hx-view w))) (and v (hexv-filename v) (list (namestring (hexv-filename v))))))
(defmethod window-restore-state ((w hex-window) state)
  (let ((v (%hx-view w)) (fn (if (consp state) (first state) state)))
    (when (and v fn (probe-file fn)) (hex-load v fn))))

;;; --- entry points -----------------------------------------------------------

(defun make-hexdump (&optional path)
  "Build a hex-editor window for PATH (or an empty buffer).  Returns (values WINDOW
FOCUS OPEN) per the framework's builder contract; OPEN is a no-op (the file, if any,
is loaded eagerly)."
  (let* ((win  (make-instance 'hex-window :title " hexdump " :keymap *global-keys*))
         (body (make-instance 'stack))
         (hv   (make-instance 'hex-view :name 'hex))
         (st   (make-instance 'static-text :name 'status :role :status :text "")))
    (add-laid body hv :fill)
    (add-laid body st 1)
    (add-subview win body)
    (setf (window-scroll-target win) hv (window-help win) :hexdump)
    (if (and path (probe-file path))
        (hex-load hv path)
        (setf (hexv-mode hv) :insert))              ; a new, empty buffer starts ready to type
    ;; OPEN returns a cleanup thunk: close any paged file source when the window closes.
    (values win hv (lambda (s) (declare (ignore s)) (lambda () (%close-source hv))))))

(defun run-hexdump (&optional path)
  "Run a hex editor full-screen for PATH (or an empty buffer) until it quits."
  (multiple-value-bind (win focus open) (make-hexdump path)
    (run-view win :focus focus :open open)))

;;; --- desktop integration -----------------------------------------------------

(defun open-hexdump (dt path)
  "Open (or re-target the existing) hex-editor window on desktop DT for PATH."
  (let ((existing (find-if (lambda (w) (typep w 'hex-window)) (dt-windows dt))))
    (cond
      (existing (hex-load (%hx-view existing) path)
                (dt-raise dt existing) (dt-refocus dt) (invalidate dt))
      (t (dt-open dt (lambda () (make-hexdump path)))
         ;; opened via a builder function -> record the KIND ourselves so the window
         ;; (and its file, via window-save-state) is saved with the layout.
         (let ((win (dt-top dt))) (when win (setf (window-kind win) :hexdump)))))))

(defun prompt-hexdump (dt)
  "Prompt for a file and open it in a hex-editor window on desktop DT."
  (let ((p (make-file-dialog :dir *project-dir* :title " Open file (hex) ")))
    (when p (open-hexdump dt p))))

(defvar *auto-menu* t
  "When true (the default), loading this system contributes a `Tools ▸ Hex editor…' item to
the desktop menu bar — so it works on the bare `revision' desktop with no extra wiring.  A
host that curates its own menu (placing the item itself) sets this to NIL so it isn't added
twice.  Checked when the desktop builds its menus.")

;; register the builder (for dt-open + layout save/restore) and, unless a host
;; opts out, a Tools item that prompts for a file.
(register-window :hexdump (lambda () (make-hexdump)))
(register-menu :hexdump
      (lambda (dt)
        (when *auto-menu*
          (list "Tools" (list "Hex editor…" (lambda () (prompt-hexdump dt)))))))
