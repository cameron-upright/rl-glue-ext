#!/bin/bash
if [ -z $1 ]
then
  VERSION=UNKNOWN
  echo "You didn't pass a version so using UNKNOWN"
else
  VERSION=$1
fi

DEVDIR=matlab-codec
DISTDIR=dist

rm -Rf $DEVDIR
rm -Rf $DISTDIR
svn export http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Matlab $DEVDIR

mkdir $DISTDIR
mv $DEVDIR/docs/MatlabCodec.pdf $DEVDIR/
rm -Rf $DEVDIR/docs

DEVTAR=$DEVDIR-$VERSION.tar

tar -cf $DEVTAR $DEVDIR
gzip $DEVTAR

mv *.gz $DISTDIR

rm -Rf $DEVDIR

