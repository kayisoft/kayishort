;;;; al-tobeh-website.asd

(asdf:defsystem kayishort
  :name "KayiShort"
  :version "0.0.1"
  :author "Mohammad Matini <mohammad.matini@outlook.com>"
  :license  "Unlicensed"
  :description "A simple URL shortner and resolving them later."
  :homepage "https://kayisoft.net/"
  :serial t
  :depends-on (:cl-dbi                  ;sqlite db access
               :sxql                    ;sql query generator
               :verbose                 ;logging framework
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
               (:file "shortner")       ;main application logic
               ))

;; (loop for dependecy in
;;      '(:sxql :verbose :ironclad :clack :cl-ppcre
;;        :http-body :cl-json :cl-dbi)
;;      do (ql:quickload dependecy))
