#!/bin/bash
#
# Download Lisp
#
if [ -z $1 ]
then
  VERSION=UNKNOWN
  echo "You didn't pass a version so using UNKNOWN"
else
  VERSION=$1
fi

DEVDIR=lisp-codec
DISTDIR=dist

rm -Rf $DEVDIR
rm -Rf $DISTDIR
svn export http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Lisp $DEVDIR

mkdir $DISTDIR
cp $DEVDIR/doc/manual/lisp-codec.pdf $DEVDIR/

DEVTAR=$DEVDIR-$VERSION.tar

tar -cf $DEVTAR $DEVDIR
gzip $DEVTAR
mv *.gz $DISTDIR/
ZIPNAME=$DEVTAR.gz
DEVZIP=$DISTDIR/$ZIPNAME

rm -Rf $DEVDIR

python ../googlecode_upload.py -s "RL-Glue Lisp Codec $VERSION" -p rl-glue-ext --labels=Type-Installer,OpSys-All,Language-Lisp,Audience-User,Audience-Dev $DEVZIP


# Update the Wiki
python substitute-lisp-strings.py $VERSION $DEVDIR-$VERSION
cp Lisp.wiki ../wiki/lisp.new
cd ../wiki
svn up
mv lisp.new Lisp.wiki
svn commit Lisp.wiki -m "Automated update of Lisp wiki page."
