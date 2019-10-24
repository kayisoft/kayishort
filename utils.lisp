;;;; utils.lisp

(in-package #:net.kayisoft.kayishort)

(defconstant +url-id-characters+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._"
  "Sequence of characters to draw from when generating unique path IDs.

We kept off characters that require URL encoding. This pool is large
enough for our purposes.")

(defconstant +url-validation-regex+
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

(defun valid-url-p (url)
  "Validate a URL string."
  (cl-ppcre:scan +url-validation-regex+ url))

(defun generate-random-url-id (length)
  "Returns a random string that doesn't require URL encoding."
  (loop with id = (make-string length) for i below length
     do (setf (aref id i)
              (aref +url-id-characters+
                    (crypto:strong-random
                     (length +url-id-characters+))))
     finally (return id)))
