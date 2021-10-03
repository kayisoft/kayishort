;;;; config.lisp --- configuration parameters

;;; Copyright (C) 2020 Kayisoft, Inc.

;;; Author: Mohammad Matini <mohammad.matini@outlook.com>
;;; Maintainer: Mohammad Matini <mohammad.matini@outlook.com>

;;; Description: This file defines various configuration parameters for
;;; Kayishort; server port, SQLite database path, and API access
;;; token. Also, it includes utilities to refresh those values at run-time
;;; for better interactive development experience.

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
;;;                                   Server
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defparameter *api-access-token*
  (or (uiop:getenv "KAYISHORT_API_TOKEN")
      "IU89SyvaHF++1czA8ZBzIxhqQbpa2CyO/bk=")
  "Password for API authentication.")

;;; --------------------------------------------------------------------------
(defparameter *server-port*
  (parse-integer (or (uiop:getenv "KAYISHORT_SERVER_PORT") "80") :junk-allowed t)
  "Port on which the server will listen.")

;;; --------------------------------------------------------------------------
;;;                                  Database
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defparameter *database-path*
  (or (uiop:getenv "KAYISHORT_DATABASE_PATH") "./data/store.db")
  "The path for the sqlite3 database.")

;;; --------------------------------------------------------------------------
;;;                                Utilities
;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defun refresh-config-from-current-env ()
  "Loads the latest state of configuration environmental variables"
  (let ((api-access-token (uiop:getenv "KAYISHORT_API_TOKEN"))
        (server-port (uiop:getenv "KAYISHORT_SERVER_PORT"))
        (database-path (uiop:getenv "KAYISHORT_DATABASE_PATH")))
    (when api-access-token (setf *api-access-token* api-access-token))
    (when server-port (setf *server-port* (parse-integer server-port :junk-allowed t)))
    (when database-path (setf *database-path* database-path))))
