#!/bin/bash
INSTALLDIR=`pwd`/install_root/usr/local
rm -Rf rl-glue-trunk
rm -Rf install_root
mkdir install_root
mkdir install_root/usr
mkdir install_root/usr/local
svn export http://rl-glue.googlecode.com/svn/trunk rl-glue-trunk
cd rl-glue-trunk
#Disable shared so that we can package them up and relocate the libraries.
./configure --prefix=$INSTALLDIR --disable-shared
make
make install
cd ..

