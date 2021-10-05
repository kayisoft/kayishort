;;;; server.lisp --- thin abstraction over clack

;;; Copyright (C) 2020 Kayisoft, Inc.

;;; Author: Mohammad Matini <mohammad.matini@outlook.com>
;;; Maintainer: Mohammad Matini <mohammad.matini@outlook.com>

;;; Description: This file provides a simple thin abstraction over Clack;
;;; providing simple functions to start and stop the server, and the
;;; `defapi' function to define API endpoints and handlers.

;;; This file is part of Kayishort.

;;; Kayishort is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; Kayishort is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Kayishort. If not, see <https://www.gnu.org/licenses/>.

(in-package #:net.kayisoft.kayishort)

;;; --------------------------------------------------------------------------
;;;                             Server State
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defvar *server* nil)
(defparameter *api-definitions* nil)

;;; --------------------------------------------------------------------------
;;;                             Server Utilities
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defun main ()
  (refresh-config-from-current-env)
  (start-server)
  #+(or sbcl ccl clisp ecl allegro)
  (handler-case (join-server-thread)
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      ()
      (progn (format *error-output* "~%Aborting.~&")
             (stop-server) (uiop:quit)))
    (error (c) (format t "Unknown error occured:~&~a~&" c)))
  #-(or sbcl ccl clisp ecl allegro) (join-server-thread))

;;; --------------------------------------------------------------------------
(defun join-server-thread ()
  (bt:join-thread
   (find-if (lambda (thread) (search "hunchentoot" (bt:thread-name thread)))
            (bt:all-threads))))

;;; --------------------------------------------------------------------------
(defun start-server ()
  (setf *server* (clack:clackup
                  (lambda (env) (funcall #'dispatch-request env))
                  :address "0.0.0.0" :port *server-port*)))

;;; --------------------------------------------------------------------------
(defun stop-server () (clack:stop *server*) (setf *server* nil))

;;; --------------------------------------------------------------------------
;;;                             Request Handling
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defun dispatch-request (environment)
  (let* ((api-handler (getf (find-matching-api environment) :handler))
         (response (generate-response api-handler environment)))
    (format t "[RESPONSE] ~A ~A ~A~%"
            (getf environment :request-method)
            (getf environment :path-info)
            (car response))
    response))

;;; --------------------------------------------------------------------------
(defun generate-response (api-handler environment)
  "Attempt to call `api-handler' with the clack `environment' as a parameter,
returning a 500 response on errors, and a 404 response if api-handler was
nil."
  (handler-bind
      ((error (lambda (err) (trivial-backtrace:print-backtrace err)
                      (return-from generate-response
                        `(500 nil ("Internal Server Error"))))))
    (return-from generate-response
      (or (when api-handler (funcall api-handler environment))
          `(404 nil ("Not Found"))))))

;;; --------------------------------------------------------------------------
;;;                             API Utilities
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defun find-matching-api (environment)
  "Returns the appropriate API definition from the special variable
`*api-definitions*' based on the provided clack `environment'."
  (find-if
   (lambda (handler)
     (and (equal (getf handler :method) (getf environment :request-method))
          (cl-ppcre:scan (getf handler :path) (getf environment :path-info))))
   *api-definitions*))

;;; --------------------------------------------------------------------------
(defun defapi (method path handler)
  "Bind a new `handler' function to an API `path' on HTTP
`method'. `path' is a PPCRE regular expression matched against the
request path. If multiple bindings matched by path and had the same
method, then the most recent one shadows the rest."
  (push (list :method method :path path :handler handler)
        *api-definitions*))
