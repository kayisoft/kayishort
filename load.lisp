;;;; load.lisp
;;; Loading this file loads our system into the lisp image after
;;; ensuring the installation of required dependencies.

(load "./load-dependencies.lisp")
(asdf:load-system :kayishort)
