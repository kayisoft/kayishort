# Kayishort
### Mohammad Matini <mohammad.matini@outlook.com>

A simple SQLite-based URL shortner.

## Install
The quickest way for development is:

* install sqlite3 for your distro
* install sbcl: http://www.sbcl.org/
* install quicklisp: https://www.quicklisp.org/beta/
* start a repl in the project's directory.
* quickload all dependencies, they are listed in the ASDF system
   definition file `kayishort.asd`, for example:
```common-lisp
       (loop for dependecy in
            '(:sxql :verbose :ironclad :clack :cl-ppcre
              :http-body :cl-json :cl-dbi)
            do (ql:quickload dependecy))
```
* Modify the config in `config.lisp`. Set a new password.
* load the `load.lisp` file
```common-lisp
       (load "./load.lisp")
```
* If this is your first time running the application, or if new
   database migrations are committed, call `(migrate-latest)` (This
   will also create the database file if it does not exist).

* profit.

## License

Unlicensed

