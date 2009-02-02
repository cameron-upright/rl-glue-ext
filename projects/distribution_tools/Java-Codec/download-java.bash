#!/bin/bash
USERDIR=java-codec
DEVDIR=$USERDIR-dev
DISTDIR=dist
rm -Rf $USERDIR
rm -Rf $DEVDIR
rm -Rf $DISTDIR
svn export http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java $DEVDIR

mkdir $DISTDIR
mkdir $USERDIR
cp $DEVDIR/products/JavaRLGlueCodec.jar $USERDIR
cp $DEVDIR/docs/JavaCodec.pdf $USERDIR
mv $DEVDIR/USER-README $USERDIR/README
mv $DEVDIR/DEV-README $DEVDIR/README
cp -R $DEVDIR/examples $USERDIR/examples
java org.rlcommunity.rlglue.codec.RLGlueCore --uninstall
VERSION=`java -jar $USERDIR/JavaRLGlueCodec.jar --version`

DEVTAR=$DEVDIR-$VERSION.tar
USERTAR=$USERDIR-$VERSION.tar

tar -cf $DEVTAR $DEVDIR
tar -cf $USERTAR $USERDIR
gzip $DEVTAR
gzip $USERTAR

mv *.gz $DISTDIR

rm -Rf $USERDIR
rm -Rf $DEVDIR

