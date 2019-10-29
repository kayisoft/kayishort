;;;; build.lisp
;;; Loading this file builds an executable binary core image of our
;;; application after ensuring the installation of required
;;; dependencies.

(load "./load-dependencies.lisp")
(asdf:make :kayishort)
