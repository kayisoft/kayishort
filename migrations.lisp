;;;; migrations.lisp --- migration definitons for SQLite

;;; Copyright (C) 2020 Kayisoft, Inc.

;;; Author: Mohammad Matini <mohammad.matini@outlook.com>
;;; Maintainer: Mohammad Matini <mohammad.matini@outlook.com>

;;; Description: This file contains the SQL migration definitions required
;;; to setup Kayishort's database schema. Database migrations are executed
;;; by the data helpers in data.lisp

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
