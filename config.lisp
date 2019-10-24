;;;; config.lisp

(in-package #:net.kayisoft.kayishort)

(defparameter *shortening-domain* "s.kayisoft.net"
  "Domain used to generate short URLs.")

(defparameter *api-access-password* "IU89SyvaHF++1czA8ZBzIxhqQbpa2CyO/bk="
  "Password for API authentication.")

(defparameter *database-path* "./data/store.db"
  "The path for the sqlite3 database.")
