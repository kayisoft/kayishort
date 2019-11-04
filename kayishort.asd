;;;; al-tobeh-website.asd

(asdf:defsystem kayishort
  :name "KayiShort"
  :version "0.0.1"
  :author "Mohammad Matini <mohammad.matini@outlook.com>"
  :license  "Unlicensed"
  :description "A simple URL shortner and resolving them later."
  :homepage "https://kayisoft.net/"
  :serial t
  :depends-on (:bordeaux-threads        ;portable threading lib
               :cl-dbi                  ;sqlite db access
               :sxql                    ;sql query generator
               :ironclad                ;crypto lib for secure rng
               :clack                   ;web server framework
               :cl-ppcre                ;regex library
               :http-body               ;http body parser
               :cl-json                 ;json library
               ;; :idna                 ;TODO: IDN punycode conversion
               )
  :components ((:file "package")        ;package definitions
               (:file "config")         ;applications configurations
               (:file "utils")          ;various helper utils
               (:file "migrations")     ;database migrations
               (:file "data")           ;database access
               (:file "kayishort")      ;main application logic
               )
  :build-operation "program-op"
  :build-pathname "./dist/kayishort"
  :entry-point "kayishort:main")

;;; Allows compression on sbcl
#+sb-core-compression
(defmethod asdf:perform ((operation asdf:image-op) (component asdf:system))
  (uiop:dump-image (asdf:output-file operation component)
                   :executable t :compression 9))
