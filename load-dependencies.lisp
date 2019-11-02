;;;; install-dependencies.lisp
;;; Loading this file installs all required dependencies. Quicklisp is
;;; required for this to work. On debian, install `cl-quicklisp`.

(push *default-pathname-defaults* ql:*local-project-directories*)
(load "kayishort.asd")
(ql:quickload (asdf:system-depends-on (asdf:find-system :kayishort)))
