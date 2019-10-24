# Kayishort
### Mohammad Matini <mohammad.matini@outlook.com>

A simple SQLite-based URL shortner.

## Install
The quickest way for development is:
1. install sbcl: http://www.sbcl.org/
2. install quicklisp: https://www.quicklisp.org/beta/
3. start a repl in the project's directory.
4. quickload all dependencies, they are listed in the ASDF system
   definition file (`kayishort.asd`), for example:
   ``` common-lisp
       (loop for dependecy in
            '(:sxql :verbose :ironclad :clack :cl-ppcre
              :http-body :cl-json :cl-dbi)
            do (ql:quickload dependecy))
     ```
5. Modify the config in `config.lisp`. Set a new password.
6. load the `load.lisp` file
   ``` common-lisp
   (load "./load.lisp")
   ```
7. If this is your first time running the application, or if new
   database migrations are committed, call `(migrate-latest)` (This
   will also create the database file if it does not exist).

8. profit.

## License

Unlicensed

