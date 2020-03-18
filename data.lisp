;;;; data.lisp

(in-package #:net.kayisoft.kayishort)

(defun get-all-url-records ()
  (exec-db-query (sxql:select :* (sxql:from :urls))))

(defun insert-url (id url)
  (exec-db-query (sxql:insert-into :urls (sxql:set= :id id :url url))))

(defun get-url-record-by-id (id)
  (car (exec-db-query (sxql:select (:id :url) (sxql:from :urls)
                                   (sxql:where (:= :id id))))))

(defun increment-url-visit-count (id)
  (exec-db-query (sxql:update :urls
                   (sxql:set= :visits (:+ :visits 1))
                   (sxql:where (:= :id id)))))

;;; data utils:

(defun database-connection ()
  "Returns a database connection. It uses cached connections if they
are available."
  (dbi:connect-cached :sqlite3 :database-name *database-path*))

(defun exec-db-raw-query (query &optional params)
  "Execute a raw SQL query. Optionally accepts a list of SQL parameter
bindings to bind in the query. Automatically handles creating /
caching of database connections."
  (with-connection-exec-db-raw-query (database-connection) query params))

(defun with-connection-exec-db-raw-query (connection query &optional params)
  "Execute a raw SQL query over the specified connection. Optionally
accepts a list of SQL parameter bindings to bind in the query.

You might want to use `exec-db-raw-query` instead, which automatically
handles creating / caching of database connections."
  (dbi:fetch-all
   (apply #'dbi:execute
          (append (list (dbi:prepare connection query)) params))))

(defun exec-db-migration (query-list)
  "Execute a list of raw SQL queries wrapped in a transaction."
  (let ((transaction-completed nil))
    (dbi:with-connection (connection :sqlite3 :database-name *database-path*)
      ;; We start a dedicated connection for the migration because
      ;; SQLite's transactions are connection-based. Otherwise, other
      ;; concurrent calls to the DB will be included in our
      ;; transaction if they happen concurrently.
      (with-connection-exec-db-raw-query connection "BEGIN TRANSACTION")
      (unwind-protect (loop for query in query-list
                         do (with-connection-exec-db-raw-query connection query)
                         finally (setf transaction-completed t))
        (if transaction-completed
            (with-connection-exec-db-raw-query connection "COMMIT")
            (with-connection-exec-db-raw-query connection "ROLLBACK"))))))

(defun exec-db-query (lispy-query)
  "Execute an SXQL query. SXQL is a lispy syntax for SQL."
  (multiple-value-bind (query params) (sxql:yield lispy-query)
    (exec-db-raw-query query params)))

(defun get-db-version ()
  "Gets the current migration version, which is stored in the PRAGMA
`user_version`."
  (cadar (exec-db-query (sxql:pragma "user_version"))))

(defun set-db-version (version)
  "Sets the PRAGMA `user_version` in the SQLite3 database. This is
used to track the current applied migration version."
  (exec-db-query (sxql:pragma "user_version" version)))

(defun get-migration-by-id (id)
  "Get a migration definition by its ID from the global migration
definitions list."
  (find-if (lambda (migration-id) (equal migration-id id))
           *database-migrations*
           :key (lambda (migration) (getf migration :id))))

(defun get-wanted-migration-version ()
  "Find the maximum migration ID in the global migration
definitions. Migration IDs are sequential numbers."
  (apply #'max (mapcar (lambda (migration) (getf migration :id))
                       *database-migrations*)))

(defun database-migrate-latest ()
  "Migrate the database to the latest migration version. It only
executes migrations that were not executed before. Also, it creates
the database automatically if it does not already exist."
  (ensure-directories-exist *database-path*)
  (let ((current-database-version (get-db-version))
        (wanted-database-version (get-wanted-migration-version)))
    (when (< current-database-version wanted-database-version)
      (loop for migration-id
         from (1+ current-database-version) to wanted-database-version
         do (exec-db-migration
             (getf (get-migration-by-id migration-id) :up))
           (set-db-version migration-id)))))
