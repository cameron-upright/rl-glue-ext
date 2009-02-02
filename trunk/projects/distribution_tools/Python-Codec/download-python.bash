#!/bin/bash
USERDIR=python-codec
DEVDIR=$USERDIR-dev
DISTDIR=dist
rm -Rf $USERDIR
rm -Rf $DEVDIR
rm -Rf $DISTDIR
svn export http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Python $DEVDIR
VERSION=`PYTHONPATH=$DEVDIR/src/ python get_version.py`

cp -R $DEVDIR $USERDIR
mkdir $DISTDIR

mv $USERDIR/docs/PythonCodec.pdf $USERDIR/
rm -Rf $USERDIR/docs
mv $USERDIR/USER-README $USERDIR/README
rm $USERDIR/DEV-README
mv $DEVDIR/DEV-README $DEVDIR/README
rm $DEVDIR/USER-README

DEVTAR=$DEVDIR-$VERSION.tar
USERTAR=$USERDIR-$VERSION.tar
 
tar -cf $DEVTAR $DEVDIR
tar -cf $USERTAR $USERDIR
gzip $DEVTAR
gzip $USERTAR
 
mv *.gz $DISTDIR

rm -Rf $USERDIR
rm -Rf $DEVDIR

