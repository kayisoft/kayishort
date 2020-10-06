# Kayishort
A simple SQLite-based URL shortner.

Current Maintainer: Mohammad Matini <<mohammad.matini@outlook.com>>

## Dependencies
1. Install sqlite3 for your distro
2. Install sbcl: http://www.sbcl.org/ (We only support `SBCL` for now)
3. Install quicklisp: https://www.quicklisp.org/beta/ and ensure it
   gets loaded in your `.sbclrc`.

## Build & Run
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

## Docker
To build the docker image, run the following command in the project's
directory (change the name and tag if you want):

```
docker build . --tag kayishort:0.1
```

To run a container of this image, use the following command. Replace
any values appropriately, and see the next section for other env-vars
you can use:

```
docker run -p "8000:80" \
           -e KAYISHORT_API_TOKEN=RfIkcLRmnyGkBN/6mujRhtxhL5r9azdVnuk= \
           -v "/home/yourhome/kayishort-data/:/app/data/" --rm -it kayishort:0.1
```

## Environmental Variables

<table>
  <thead>
    <tr>
      <th scope="col">VARIABLE</th>
      <th scope="col">DESCRIPTION</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>KAYICORRECT_API_TOKEN</code></td>
      <td> API access token. Clients that want to create new short urls must
        send the same token in their <code>Authorization</code>
        header. </td>
    </tr>
    <tr>
      <td><code>KAYICORRECT_SERVER_PORT</code></td>
      <td> Server listening port. <code>80</code> by default. </td>
    </tr>
    <tr>
      <td><code>KAYISHORT_DATABASE_PATH</code></td>
      <td> The path to the sqlite3 database file. The database file is
        automatically created if it does not exist, and migrations are
        automatically run on server startup. </td>
    </tr>
  </tbody>
</table>
    
## API

### Create Short URL

`POST` `/urls`

Create a new short URL. The original long URL is specified as a string value
of the property `originalUrl` within a JSON POST body payload. The URL must
be valid, and must begin with `http://` or `https://`. An example of the
POST request body might be:

```json
{ "originalUrl": "http://my-long-url.com" }
```

The response returned by the server will contain the generated short URL as
the string value of the property `shortUrl` within a JSON response body. An
example of the response body might be:

```json
{ "shortUrl": "localhost:8000\/urls\/aTlrBBaUrCKQ" }
```

You can then visit the resulting short URL in a web browser, and you will be
redirected to the long URL.

Notes:

- The API endpoint communicates in `Content-Type: application/json` only.
- This API endpoint requires Authorization. To authorize, you must set
  the `Authorization` header to `Bearer {{KAYICORRECT_API_TOKEN}}`.

### Visit Short URL

`GET` `/urls/:id`

When a short url is requested, the server will respond with a `301`
redirect to the actual original URL. Web browsers would automatically
follow the redirect to that original page.

## Limitations
* Currently does not support non-ascii URLs. (i.e. URLs containing
  symbols or Chinese or Arabic characters, etc.)

* Only supports the `SBCL` Common-Lisp implementation at the moment.

## License
Copyright (C) 2020 Kayisoft Inc.

Kayishort is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Kayishort is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Kayishort. If not, see <https://www.gnu.org/licenses/>.
