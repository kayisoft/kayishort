;;;; migrations.lisp

(in-package #:net.kayisoft.kayishort)

(defparameter *database-migrations*
  '((:up "
CREATE TABLE IF NOT EXISTS urls (
id VARCHAR(12) UNIQUE,
url TEXT,
created_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL)
"
     :down "
DROP TABLE urls
")))
