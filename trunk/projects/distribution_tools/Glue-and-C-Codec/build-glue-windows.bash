#!/bin/bash
#Assume the glue is downloaded to rl-glue-trunk and installed to install_root
INSTALLDIR=`pwd`/install_root/usr/local
VERSION=`$INSTALLDIR/bin/rl_glue --pv`
#Created by download-build-glue
cd rl-glue-trunk

#configure for cross compile
./configure --host i586-mingw32msvc --enable-mingw
make
BinaryName=RL-Glue-Windows-Binary-$VERSION.zip
cp rl-glue-trunk/docs/Glue-Overview.pdf .
cp rl-glue-trunk/docs/TechnicalManual.pdf .
zip $BinaryName src/rl_glue.exe docs/Glue-Overview.pdf docs/TechnicalManual.pdf
cp $BinaryName ../dist/



