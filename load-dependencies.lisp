;;;; install-dependencies.lisp
;;; Loading this file installs all required dependencies. Quicklisp is
;;; required for this to work. On debian, install `cl-quicklisp`.

(push *default-pathname-defaults* ql:*local-project-directories*)
(load "kayishort.asd")
(loop for package in (asdf:system-depends-on (asdf:find-system :kayishort))
   do (ql:quickload package))
