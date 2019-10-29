FROM debian:10

# Install OS dependencies
RUN apt-get update &&\
        DEBIAN_FRONTEND=noninteractive apt-get install -yq \
        sbcl cl-quicklisp rlwrap sqlite3 &&\
        DEBIAN_FRONTEND=noninteractive apt-get clean &&\
        rm -rf /var/lib/apt/lists/*

# Install quicklisp with a specific dist, add it to sbcl init file.
RUN sbcl --noinform --non-interactive --load "/usr/share/common-lisp/source/quicklisp/quicklisp.lisp" \
        --eval "(quicklisp-quickstart:install :dist-url \"http://beta.quicklisp.org/dist/quicklisp/2019-10-08/distinfo.txt\")" \
        --eval '(ql-util:without-prompting (ql:add-to-init-file))'

# Install common lisp application dependencies.
COPY ./kayishort.asd ./load-dependencies.lisp /app/
WORKDIR /app
RUN sbcl --noinform --non-interactive --load "./load-dependencies.lisp"

# Build executable binary image
COPY . /app
RUN sbcl --noinform --non-interactive --load "./build.lisp"

ENTRYPOINT ["./dist/kayishort"]
