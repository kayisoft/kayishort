# Kayishort
### Mohammad Matini <mohammad.matini@outlook.com>

A simple SQLite-based URL shortner.

## Dependencies
1. Install sqlite3 for your distro
2. Install sbcl: http://www.sbcl.org/ (We only support `SBCL` for now)
3. Install quicklisp: https://www.quicklisp.org/beta/ and ensure it
   gets loaded in your `.sbclrc`.

## Build
To build an executable binary:

1. In the project's directory, run: `sbcl --load "build.lisp"`. This
   will download all of our Common-Lisp dependencies, and build an
   executable image.
2. run the resulting `dist/kayishort` file with the appropriate
   environmental variables for configurations.

## Development
To load the system for interactive development:

1. Load the `load.lisp` file which will download all of our
   Common-Lisp dependencies, and load the kayishort system into your
   lisp image.
2. See `config.lisp` for setf-able configs, `setf` them to new values
   if needed.
3. On the repl, run `(start-server)` to start the server.
4. Hack away...
5. Run `(stop-server)` to stop the server.

## Environmental Variables

### `KAYISHORT_API_PASSWORD`
    API password. Clients that want to create new short urls must send
    the same password in their `Authorization` header.

### `KAYISHORT_SERVER_PORT`
    Server listening port. `80` by default.
    
### `KAYISHORT_DATABASE_PATH`
    The path to the sqlite3 database file. The database file is
    automatically created if it does not exist, and migrations
    automatically run, on server startup.
    
## API

### `POST` `/urls` `create-short-url`
To create a new short URL. The API communicates using the
`Content-Type: application/json`. 

This API endpoint requires Authorization. To authorize, you must set
the `Authorization` header to `Bearer {{KAYISHORT_API_PASSWORD}}`.

The URL you want to shorten should be sent in the POST body as a
string value of the property `originalUrl`. For example:
`{"originalUrl": "http://google.com"}` Notice that the URL must be
valid, and have `http://` or `https://` at the beginning.

The response will contain the generated short URL as a string value of
the property `shortUrl`. For example:
`{"shortUrl":"localhost:8000\/urls\/aTlrBBaUrCKQ"}`

Then you can visit the resulting short URL in a browser.

### `GET` `/urls/:id` `get-short-url`
When a short url is requested, the server will respond with a `301`
redirect to the actual original URL. Web browsers would automatically
follow the redirect to that original page.

## Limitations
* Currently does not support non-ascii URLs. (i.e. URLs containing
symbols or Chinese or Arabic characters, etc.)

* Only supports the `SBCL` Common-Lisp implementation.

## License
Unlicensed

