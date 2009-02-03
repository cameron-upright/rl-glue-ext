#!/bin/bash
#
# Download Matlab
#
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
mv *.gz $DISTDIR/
ZIPNAME=$DEVTAR.gz
DEVZIP=$DISTDIR/$ZIPNAME

rm -Rf $DEVDIR

python ../googlecode_upload.py -s "RL-Glue Matlab Codec $VERSION" -p rl-glue-ext --labels=Type-Installer,OpSys-All,Language-Matlab $DEVZIP

fileURL=http://rl-glue-ext.googlecode.com/files/$ZIPNAME

# Update the Wiki
python substitute-matlab-strings.py $VERSION $fileURL
cp Matlab.wiki ../wiki/
cd ../wiki
svn commit Matlab.wiki -m "Automated update of Matlab wiki page."
