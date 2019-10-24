;;;; load.lisp

(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :kayishort)
(kayishort:start)
