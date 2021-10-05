;;;; kayishort.asd --- ASDF system definition file

;;; Copyright (C) 2020 Kayisoft, Inc.

;;; Author: Mohammad Matini <mohammad.matini@outlook.com>
;;; Maintainer: Mohammad Matini <mohammad.matini@outlook.com>

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

(asdf:defsystem kayishort
  :name "Kayishort"
  :version "0.0.1"
  :author "Mohammad Matini <mohammad.matini@outlook.com>"
  :license  "GPLv3"
  :description "A simple SQLite-based URL shortner."
  :homepage "https://github.com/kayisoft/kayishort"
  :serial t
  :depends-on (:bordeaux-threads        ;portable threading lib
               :cl-dbi                  ;sqlite db access
               :sxql                    ;sql query generator
               :ironclad                ;crypto lib for secure rng
               :clack                   ;web server framework
               :trivial-backtrace       ;portable condition backtraces
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
               (:file "server")         ;various server utilities
               (:file "kayishort")      ;main API definitions
               )
  :build-operation "program-op"
  :build-pathname "./dist/kayishort"
  :entry-point "kayishort:main")

;;; Allows compression on sbcl
#+sb-core-compression
(defmethod asdf:perform ((operation asdf:image-op) (component asdf:system))
  (uiop:dump-image (asdf:output-file operation component)
                   :executable t :compression 9))
