;;;; config.lisp

(in-package #:net.kayisoft.kayishort)

(defparameter *api-access-token*
  (or (uiop:getenv "KAYISHORT_API_TOKEN")
      "IU89SyvaHF++1czA8ZBzIxhqQbpa2CyO/bk=")
  "Password for API authentication.")

(defparameter *server-port*
  (parse-integer (or (uiop:getenv "KAYISHORT_SERVER_PORT") "80") :junk-allowed t)
  "Port on which the server will listen.")

(defparameter *database-path*
  (or (uiop:getenv "KAYISHORT_DATABASE_PATH") "./data/store.db")
  "The path for the sqlite3 database.")

(defun refresh-config-from-current-env ()
  "Loads the latest state of configuration environmental variables"
  (let ((api-access-token (uiop:getenv "KAYISHORT_API_TOKEN"))
        (server-port (uiop:getenv "KAYISHORT_SERVER_PORT"))
        (database-path (uiop:getenv "KAYISHORT_DATABASE_PATH")))
    (when api-access-token (setf *api-access-token* api-access-token))
    (when server-port (setf *server-port* (parse-integer server-port :junk-allowed t)))
    (when database-path (setf *database-path* database-path))))
