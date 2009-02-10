#!/bin/bash
USERDIR=python-codec
VERSION=`PYTHONPATH=$USERDIR/src/ python get_version.py`
USERZIP=$USERDIR-$VERSION-win32.zip
DISTDIR=dist
WINDIR=python-codec-windows
rm -Rf $WINDIR
cp -R $USERDIR $WINDIR

cd $WINDIR/src
python setup.py bdist_wininst
cp dist/RL_Glue_PythonCodec-$VERSION.win32.exe ../
cd ..
rm -Rf src
zip $USERZIP PythonCodec.pdf *.exe README.txt	examples	LICENSE-2.0.txt
cp $USERZIP ../dist



