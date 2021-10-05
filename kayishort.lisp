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

;;; --------------------------------------------------------------------------
;;;                               Handler Definitions
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defun get-urls-handler (environment)
  (let* ((id (car (last (cl-ppcre:split "/urls/" (getf environment :path-info)))))
         (redirect-url (get-url-by-id id)))
    (when redirect-url
      (increment-url-visit-count id)
      `(301 (:location ,redirect-url) ("")))))

;;; --------------------------------------------------------------------------
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

;;; --------------------------------------------------------------------------
;;;                               Helpers
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defun register-short-url (url)
  (let ((generated-unique-id
         (loop for id = (generate-random-url-id 12)
            while (get-url-by-id id) finally (return id))))
    (insert-url generated-unique-id url)
    generated-unique-id))

;;; --------------------------------------------------------------------------
(defun get-url-by-id (id) (getf (get-url-record-by-id id) :|url|))

;;; --------------------------------------------------------------------------
;;;                               API Definitions
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defapi :GET  "^/urls/(.{12})$" #'get-urls-handler)
(defapi :POST "^/urls/?$"       #'post-urls-handler)
