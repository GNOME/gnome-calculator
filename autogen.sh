#!/bin/sh
# Run this to generate all the initial makefiles, etc.

test -n "$srcdir" || srcdir=`dirname "$0"`
test -n "$srcdir" || srcdir=.

olddir=`pwd`
cd "$srcdir"

(test -f $srcdir/configure.ac) || {
        echo "**Error**: Directory "\`$srcdir\'" does not look like the top-level project directory"
        exit 1
}

PKG_NAME=`autoconf --trace 'AC_INIT:$1' "$srcdir/configure.ac"`

if [ "$#" = 0 -a "x$NOCONFIGURE" = "x" ]; then
        echo "**Warning**: I am going to run \`configure' with no arguments." >&2
        echo "If you wish to pass any to it, please specify them on the" >&2
        echo \`$0\'" command line." >&2
        echo "" >&2
fi

set -x
aclocal --install || exit 1
intltoolize --force --copy --automake || exit 1
autoreconf --verbose --force --install -Wno-portability || exit 1
{ set +x; } 2>/dev/null

cd "$olddir"
test -n "$NOCONFIGURE" || "$srcdir/configure" "$@"


