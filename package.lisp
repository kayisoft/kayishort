;;;; package.lisp

(in-package #:cl-user)
(defpackage #:kayishort
  (:nicknames #:net.kayisoft.kayishort)
  (:use :cl) (:export main start-server stop-server))
