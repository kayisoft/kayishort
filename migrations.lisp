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
     :down ("ALTER TABLE urls DROP COLUMN visits"))

    (:id 3
     :date "2019-12-09T16:58:40Z"
     :description "Mark `id` as a PRIMARY KEY column. SQLite does not
     fully support ALTER table, so we have to recreate the table see:
     https://www.sqlite.org/lang_altertable.html for more information"
     :up ("CREATE TABLE IF NOT EXISTS new_urls (
           id VARCHAR(12) NOT NULL PRIMARY KEY,
           url TEXT,
           visits INTEGER DEFAULT 0,
           created_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL)"

          "INSERT INTO new_urls SELECT id,url,0,created_at FROM urls"
          "DROP TABLE urls"
          "ALTER TABLE new_urls RENAME TO urls")

     :down ("CREATE TABLE IF NOT EXISTS new_urls (
             id VARCHAR(12) UNIQUE,
             url TEXT,
             visits INTEGER DEFAULT 0,
             created_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL)"

            "INSERT INTO new_urls SELECT id,url,created_at,visits FROM urls"
            "DROP TABLE urls"
            "ALTER TABLE new_urls RENAME TO urls"))))
