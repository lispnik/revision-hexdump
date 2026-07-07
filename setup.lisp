;;;; setup.lisp --- put the sibling `revision' checkout (and its vendored fiveam
;;;; for the test suite) on the ASDF registry, so
;;;; (asdf:load-system :revision-hexdump) just works.
;;;;
;;;; Usage:   sbcl --load setup.lisp --eval '(asdf:load-system :revision-hexdump)'
;;;;
;;;; Layout assumed (siblings under one parent directory):
;;;;   .../revision-hexdump/   <- this project
;;;;   .../revision/           <- the framework (no external deps)

(require :asdf)

(let* ((here (or *load-truename* *load-pathname* *default-pathname-defaults*))
       (root (make-pathname :directory (butlast (pathname-directory here))
                            :name nil :type nil :defaults here))
       (revision (merge-pathnames "revision/" root)))
  (flet ((reg (dir)
           (when (probe-file dir)
             (pushnew (truename dir) asdf:*central-registry* :test #'equal)))
         (reg-tree-append (dir)
           ;; register every .asd-containing dir at LOWEST priority (test-only deps
           ;; like fiveam, so they never shadow a higher-priority system)
           (dolist (asd (directory (merge-pathnames "**/*.asd" dir)))
             (let ((d (make-pathname :directory (pathname-directory asd) :name nil :type nil)))
               (unless (member d asdf:*central-registry* :test #'equal)
                 (setf asdf:*central-registry*
                       (append asdf:*central-registry* (list d))))))))
    (reg (make-pathname :directory (pathname-directory here) :name nil :type nil :defaults here))
    (reg revision)
    ;; the framework's vendored systems (fiveam + its deps) for the test suite
    (reg-tree-append (merge-pathnames "systems/" revision))))

(format t "~&; revision-hexdump registry ready.  (asdf:load-system :revision-hexdump)~%")
