;;;; utils.lisp --- various utilities and helper functions

;;; Copyright (C) 2020 Kayisoft, Inc.

;;; Author: Mohammad Matini <mohammad.matini@outlook.com>
;;; Maintainer: Mohammad Matini <mohammad.matini@outlook.com>

;;; Description: Contains an assorted collection of utility and helper
;;; functions used internally throughout Kayishort.

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
(defvar *url-id-characters*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "Sequence of characters to draw from when generating unique path IDs.

We kept off characters that require URL encoding. This pool is large
enough for our purposes.")

;;; --------------------------------------------------------------------------
(defvar *url-validation-regex*
  (concatenate
   'string
   ;; protocol identifier
   "^(?:(?:(?:https?):)?\\/\\/)"
   ;; IP execlusion for private/local nets
   "(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})" 
   "(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})"
   "(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})"
   ;; IP address dotted notation octets
   ;; excludes loopback network 0.0.0.0
   ;; excludes reserved space >= 224.0.0.0
   ;; excludes network & broadcast addresses
   ;; (first & last IP address of each class)
   "(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])"
   "(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5]))"
   "{2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))"
   ;; Domain names. Allow unicode. May end with dot
   "|(?:(?:[a-z0-9¡-￿][a-z0-9¡-￿_-]{0,62})?[a-z0-9¡-￿]\\.)+"
   ;; TLD name. Allow unicode. May end with dot
   "(?:[a-z¡-￿]{2,}\\.?))"
   ;; Port number
   "(?::\\d{2,5})?"
   ;; Resource paths
   "(?:[/?#]\\S*)?$")

  "PPCRE Regex used to validate URL strings.

Note: The ¡-￿ characters are used for unicode ranges. CL-PPCRE doesn't
seem to support representing wide unicode hex values as escape
codes. For more information about this regex take a look at:

* Regex we're based on (ours is modified)
  https://gist.github.com/dperini/729294

* List of regexes, and validation lists
  https://mathiasbynens.be/demo/url-regex")

;;; --------------------------------------------------------------------------
(defun valid-url-p (url)
  "Validate a URL string."
  (cl-ppcre:scan *url-validation-regex* url))

;;; --------------------------------------------------------------------------
(defun generate-random-url-id (length)
  "Returns a random string that doesn't require URL encoding."
  (loop with id = (make-string length) for i below length
     do (setf (aref id i)
              (aref *url-id-characters*
                    (crypto:strong-random
                     (length *url-id-characters*))))
     finally (return id)))

;;; --------------------------------------------------------------------------
(defun authorized-p (headers)
  "Whether or not a request's authorized based on its auth headers."
  (string= (gethash "authorization" headers)
           (concatenate 'string "Bearer " *api-access-token*)))
