#!/bin/bash
MACDIR=OSX-Package
DISTDIR=dist
VERSION=`./install_root/usr/local/bin/rl_glue --pv`
DISTNAME=RL-Glue-$VERSION-and-C-Codec
IMAGEFOLDER=$MACDIR/$DISTNAME
PACKAGENAME=$DISTNAME.pkg
IMAGENAME=$DISTNAME.dmg
mkdir $DISTDIR

rm -Rf $IMAGEFOLDER
mkdir $IMAGEFOLDER
mkdir $IMAGEFOLDER/codec
mkdir $IMAGEFOLDER/rl-glue
cp codec-trunk/docs/*.pdf $IMAGEFOLDER/codec/
cp -R codec-trunk/examples $IMAGEFOLDER/codec/

cp rl-glue-trunk/docs/*.pdf $IMAGEFOLDER/rl-glue/
cp -R rl-glue-trunk/examples $IMAGEFOLDER/rl-glue/

rm -Rf $MACDIR/*.pkg
rm $MACDIR/*.dmg
#Create the package file
/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker --root install_root --id org.rlcommunity.rlglue --version 3.0 --title "RL-Glue 3.0 and C/C++ Codec" --resources $MACDIR/resources --out $MACDIR/$PACKAGENAME --root-volume-only

mv $MACDIR/$PACKAGENAME $IMAGEFOLDER
#Create the disk image
rm $DISTDIR/$IMAGENAME
hdiutil create -srcfolder $IMAGEFOLDER $DISTDIR/$IMAGENAME
rm -Rf $IMAGEFOLDER
