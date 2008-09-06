#! /bin/sh
libtoolize --automake -c -f
aclocal -I config
autoconf
automake --foreign --add-missing --copy
