;;;; shortner.lisp

(in-package #:net.kayisoft.kayishort)

(defvar *server* nil)

(defparameter *apis*
  '((:method :GET :path "^/urls/(.{12})$" :handler get-urls-handler)
    (:method :POST :path "^/urls/?$" :handler post-urls-handler)))

(defun main ()
  (refresh-config-from-current-env)
  (start-server)
  (handler-case (sb-thread:join-thread
                 (find-if
                  (lambda (thread)
                    (search "hunchentoot" (sb-thread:thread-name thread)))
                  (sb-thread:list-all-threads)))
    ;; Catch a user's C-c
    (sb-sys:interactive-interrupt ()
      (progn
        (format *error-output* "~%Aborting.~&")
        (stop-server) (uiop:quit)))
    (error (c) (format t "Unknown error occured:~&~a~&" c))))


(defun start-server ()
  (database-migrate-latest)
  (setf *server* (clack:clackup (lambda (env) (funcall #'dispatcher env))
                                :address "0.0.0.0"
                                :port *server-port*)))

(defun stop-server () (clack:stop *server*) (setf *server* nil))

(defun dispatcher (environment)
  (handler-case
      (let* ((api (find-matching-api environment))
             (response (when api (funcall (getf api :handler) environment))))
        (if response response `(404 nil ("Not Found"))))
    (error (c) (print c) `(500 nil ("Internal Server Error")))))

(defun find-matching-api (environment)
  (find-if (lambda (handler)
             (and (equal (getf handler :method)
                         (getf environment :request-method))
                  (cl-ppcre:scan (getf handler :path)
                                 (getf environment :path-info))))
           *apis*))

(defun get-urls-handler (environment)
  (let* ((id (car (last (cl-ppcre:split "/urls/" (getf environment :path-info)))))
         (redirect-url (get-url-by-id id)))
    (when redirect-url `(301 (:location ,redirect-url) ("")))))

(defun post-urls-handler (environment)
  (destructuring-bind (&key headers content-type content-length
                            raw-body &allow-other-keys)
      environment
    (unless (authorized-p headers)
      (return-from post-urls-handler '(401 nil ("Unauthorized"))))
    (when (or (null content-length)
              (not (> content-length 0))
              (not (string-equal content-type "application/json")))
      (return-from post-urls-handler `(422 nil ("Invalid Input Value"))))
    (let* ((body (http-body:parse content-type content-length raw-body))
           (original-url (cdr (assoc "originalUrl" body :test #'string=)))
           (short-url
            (concatenate 'string (gethash "host" headers)
                         "/urls/" (register-short-url original-url))))
      (when (or (null original-url) (not (valid-url-p original-url)))
        (return-from post-urls-handler `(422 nil ("Invalid Input Value"))))
      (return-from post-urls-handler
        `(201 nil (,(cl-json:encode-json-plist-to-string
                     `(:short-url ,(format nil "~A" short-url)))))))))

(defun register-short-url (url)
  (let ((generated-unique-id
         (loop for id = (generate-random-url-id 12)
            while (get-url-by-id id) finally (return id))))
    (insert-url generated-unique-id url)
    generated-unique-id))

(defun get-url-by-id (id) (getf (get-url-record-by-id id) :|url|))

(defun authorized-p (headers)
  (string= (gethash "authorization" headers)
           (concatenate 'string "Bearer " *api-access-token*)))
