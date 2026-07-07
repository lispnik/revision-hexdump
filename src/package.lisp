;;;; package.lisp --- the REVISION-HEXDUMP package.
;;;;
;;;; An editable hex editor window for the `revision' text-mode UI framework.  The
;;;; HEX-VIEW widget renders a byte buffer as the classic three-column hexdump
;;;; (offset · hex · ASCII) and edits it in place: arrow keys move the cursor, Tab
;;;; switches the hex / ASCII pane, hex digits or printable characters overwrite
;;;; the byte under the cursor (preserving the file's size), and Ctrl-S saves.  It
;;;; doubles as a worked example of writing a custom scrollable, editable VIEW on
;;;; the framework's public widget-authoring API (DRAW-TEXT, FILL-ROW, the scroll
;;;; protocol, VIEW-KEY-HINTS, window save/restore).

(defpackage #:revision-hexdump
  (:use #:cl #:revision)
  (:documentation "An editable hex editor window (offset · hex · ASCII) for the revision framework.")
  (:export
   ;; the reusable widget + its window
   #:hex-view
   #:hex-window
   ;; entry points
   #:make-hexdump
   #:run-hexdump
   #:open-hexdump
   #:prompt-hexdump
   #:*auto-menu*
   ;; buffer logic (usable on its own, no screen needed)
   #:hex-load
   #:hex-save
   #:read-file-bytes
   #:write-file-bytes
   #:hexv-bytes
   #:hexv-length
   #:hexv-cursor
   #:hexv-pane
   #:hexv-modified
   #:hexv-filename))
