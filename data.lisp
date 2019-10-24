;;;; data.lisp

(in-package #:net.kayisoft.kayishort)

(defun database-connection ()
  (dbi:connect-cached :sqlite3 :database-name *database-path*))

(defun migrate-latest ()
  "Runs all current migrations in order.

WARNING: we don't track what migrations are run. We just run them all
unconditionally for now."
  (ensure-directories-exist *database-path*)
  (loop for migration in *database-migrations*
     do (dbi:do-sql (database-connection)
          (getf migration :up))))

(defun get-all-url-records ()
  (dbi:fetch-all
   (dbi:execute (dbi:prepare (database-connection) "SELECT * FROM urls"))))

(defun insert-url (id url)
  (multiple-value-bind (query params)
      (sxql:yield (sxql:insert-into :urls (sxql:set= :id id :url url)))
    (dbi:execute (dbi:prepare (database-connection) query)
                 (nth 0 params) (nth 1 params))))

(defun get-url-record-by-id (id)
  (multiple-value-bind (query params)
      (sxql:yield (sxql:select (:id :url) (sxql:from :urls) (sxql:where (:= :id id))))
    (dbi:fetch (dbi:execute (dbi:prepare (database-connection) query) (car params)))))
