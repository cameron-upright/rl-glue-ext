#!/bin/bash
#This file assumes that glue and codec have been built, and now it will make a mac package out of the install directory.
DISTDIR=$(pwd)/dist
VERSION=`./install_root/usr/local/bin/rl_glue --pv`
DISTNAME=RL-Glue-Debian-$VERSION
PACKAGENAME=$DISTNAME.deb
mkdir $DISTDIR
#Increment this as we release this
PACKAGERELEASE=1

cd rl-glue-trunk

./configure --host x86_32-unknown-linux-gnu
sudo make clean
make
sudo checkinstall -y --pkgname rl-glue --pkgversion $VERSION --pkgrelease $PACKAGERELEASE --pkglicense "Apache\ License\ 2.0" --pkggroup devel --maintainer "Iordanis\ Daroglou\ \<idaroglo\@csd.auth.gr\>" --provides rlglue -D
sudo make clean

./configure --host x86_64-unknown-linux-gnu
make
sudo checkinstall -y --pkgname rl-glue --pkgversion $VERSION --pkgrelease $PACKAGERELEASE --pkglicense "Apache\ License\ 2.0" --pkggroup devel --maintainer "Iordanis\ Daroglou\ \<idaroglo\@csd.auth.gr\>" --provides rlglue -D

cp *.deb $DISTDIR/

#rm -Rf $MACDIR/*.pkg
#rm $MACDIR/*.dmg
##Create the package file
#/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker --root install_root --id org.rlcommunity.rlglue --version 3.0 --title "RL-Glue 3.0 and C/C++ Codec" --resources $MACDIR/resources --out $MACDIR/#$PACKAGENAME --root-volume-only

#mv $MACDIR/$PACKAGENAME $IMAGEFOLDER
#Create the disk image
#rm $DISTDIR/$IMAGENAME
#hdiutil create -srcfolder $IMAGEFOLDER $DISTDIR/$IMAGENAME
#rm -Rf $IMAGEFOLDER
