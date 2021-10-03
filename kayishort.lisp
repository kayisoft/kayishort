;;;; kayishort.lisp --- main service logic for kayishort

;;; Copyright (C) 2020 Kayisoft, Inc.

;;; Author: Mohammad Matini <mohammad.matini@outlook.com>
;;; Maintainer: Mohammad Matini <mohammad.matini@outlook.com>

;;; Description: This file defines the HTTP API and main service logic.

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

(defvar *server* nil)

(defparameter *apis*
  '((:method :GET :path "^/urls/(.{12})$" :handler get-urls-handler)
    (:method :POST :path "^/urls/?$" :handler post-urls-handler)))

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

(defun join-server-thread ()
  (bt:join-thread
   (find-if (lambda (thread)
              (search "hunchentoot" (bt:thread-name thread)))
            (bt:all-threads))))

(defun start-server ()
  (database-migrate-latest)
  (setf *server* (clack:clackup (lambda (env) (funcall #'dispatcher env))
                                :address "0.0.0.0"
                                :port *server-port*)))

(defun stop-server () (clack:stop *server*) (setf *server* nil))

(defun dispatcher (environment)
  (handler-case
      (let* ((api (find-matching-api environment))
             (response (when api (funcall (getf api :handler) environment))))
        (if response response `(404 nil ("Not Found"))))
    (error (c) (print c) `(500 nil ("Internal Server Error")))))

(defun find-matching-api (environment)
  (find-if (lambda (handler)
             (and (equal (getf handler :method)
                         (getf environment :request-method))
                  (cl-ppcre:scan (getf handler :path)
                                 (getf environment :path-info))))
           *apis*))

(defun get-urls-handler (environment)
  (let* ((id (car (last (cl-ppcre:split "/urls/" (getf environment :path-info)))))
         (redirect-url (get-url-by-id id)))
    (when redirect-url
      (increment-url-visit-count id)
      `(301 (:location ,redirect-url) ("")))))

(defun post-urls-handler (environment)
  (destructuring-bind (&key headers content-type content-length
                            raw-body &allow-other-keys)
      environment
    (unless (authorized-p headers)
      (return-from post-urls-handler '(401 nil ("Unauthorized"))))
    (let* ((body (handler-case
                     (http-body:parse content-type content-length raw-body)
                   (error () (return-from post-urls-handler
                               `(400 nil ("Bad Request Body"))))))
           (original-url (cdr (assoc "originalUrl" body :test #'string=))))
      (when (or (null original-url) (not (valid-url-p original-url)))
        (return-from post-urls-handler `(422 nil ("Invalid Original URL"))))
      (return-from post-urls-handler
        `(201 (:content-type "application/json;charset=utf-8")
              (,(cl-json:encode-json-plist-to-string
                 `(:short-url ,(concatenate
                                'string (gethash "host" headers) "/urls/"
                                (register-short-url original-url))))))))))

(defun register-short-url (url)
  (let ((generated-unique-id
         (loop for id = (generate-random-url-id 12)
            while (get-url-by-id id) finally (return id))))
    (insert-url generated-unique-id url)
    generated-unique-id))

(defun get-url-by-id (id) (getf (get-url-record-by-id id) :|url|))

(defun authorized-p (headers)
  (string= (gethash "authorization" headers)
           (concatenate 'string "Bearer " *api-access-token*)))
