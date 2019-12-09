;;;; migrations.lisp

(in-package #:net.kayisoft.kayishort)

(defparameter *database-migrations*
  '((:id 1
     :date "2019-10-24T09:08:00Z"
     :description "Create the initial database schema"
     :up ("CREATE TABLE IF NOT EXISTS urls (
           id VARCHAR(12) UNIQUE,
           url TEXT,
           created_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL)")
     :down ("DROP TABLE urls"))

    (:id 2
     :date "2019-10-27T09:09:08Z"
     :description "Add a visit counter column"
     :up ("ALTER TABLE urls ADD visits INTEGER DEFAULT 0")
     :down ("ALTER TABLE urls DROP COLUMN visit_count"))))
