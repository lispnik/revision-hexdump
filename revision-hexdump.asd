;;;; revision-hexdump.asd --- an editable hex editor window for the `revision'
;;;; text-mode UI framework.
;;;;
;;;; A HEX-VIEW widget renders a file as the classic three-column hexdump (offset ·
;;;; hex bytes · ASCII gutter) and lets you edit it in place: move the cursor, type
;;;; hex digits or ASCII, and save.  It is a worked example of authoring a custom
;;;; scrollable, editable VIEW on the framework's public widget-authoring API.
;;;;
;;;; Depends on:
;;;;   revision   -- the CLOS-native TUI framework (sibling checkout; no external deps)
;;;;
;;;; The sibling `revision' checkout is put on the ASDF registry by ./setup.lisp;
;;;; load that first (or `make build'), then (asdf:load-system :revision-hexdump).

(asdf:defsystem "revision-hexdump"
  :description "An editable hex editor window for the revision framework."
  :author "Matthew Kennedy"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("revision")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "hexdump")
                             (:file "app")))))

;;; The test suite is a dependency-free script (like revision's own): run it with
;;;   sbcl --script tests/tests.lisp     (or `make test').
