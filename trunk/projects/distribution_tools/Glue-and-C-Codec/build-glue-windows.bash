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
mkdir tmp
cp docs/Glue-Overview.pdf ./tmp/
cp docs/TechnicalManual.pdf ./tmp/
cp src/rl_glue.exe ./tmp/
cd tmp
zip $BinaryName rl_glue.exe Glue-Overview.pdf TechnicalManual.pdf
cp $BinaryName ../../dist/
cd ..
rm -Rf tmp



