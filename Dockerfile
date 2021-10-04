# Dockerfile --- build kayishort docker image
#
# Copyright (C) 2020 Kayisoft, Inc.
#
# Author: Mohammad Matini <mohammad.matini@outlook.com>
# Maintainer: Mohammad Matini <mohammad.matini@outlook.com>
#
# Description: Builds a self-contained Debian-based Docker image of
# Kayishort, using a separate image for building the application.
#
# This file is part of Kayishort.
#
# Kayishort is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Kayishort is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Kayishort. If not, see <https://www.gnu.org/licenses/>.

FROM debian:10

# ----------------------------------------------------------------------------
#   Install OS-provided application dependencies
# ----------------------------------------------------------------------------

RUN apt-get update &&\
        DEBIAN_FRONTEND=noninteractive apt-get install -yq \
        sbcl cl-quicklisp rlwrap sqlite3 &&\
        DEBIAN_FRONTEND=noninteractive apt-get clean &&\
        rm -rf /var/lib/apt/lists/*

# ----------------------------------------------------------------------------
#   Pin quicklisp distribution & setup automatic initialization
# ----------------------------------------------------------------------------

RUN sbcl --noinform --non-interactive --load "/usr/share/common-lisp/source/quicklisp/quicklisp.lisp" \
        --eval "(quicklisp-quickstart:install :dist-url \"http://beta.quicklisp.org/dist/quicklisp/2021-06-30/distinfo.txt\")" \
        --eval '(ql-util:without-prompting (ql:add-to-init-file))'

# ----------------------------------------------------------------------------
#   Install Common-Lisp application dependencies
# ----------------------------------------------------------------------------

COPY ./kayishort.asd ./load-dependencies.lisp /app/
WORKDIR /app
RUN sbcl --noinform --non-interactive --load "./load-dependencies.lisp"

# ----------------------------------------------------------------------------
#   Build executable binary image
# ----------------------------------------------------------------------------

COPY . /app
RUN sbcl --noinform --non-interactive --load "./build.lisp"

# ----------------------------------------------------------------------------
#   Setup image entry-point
# ----------------------------------------------------------------------------

ENTRYPOINT ["./dist/kayishort"]
