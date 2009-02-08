#!/bin/bash

#Upload the Mac DMG to Google Code
VERSION=`./install_root/usr/local/bin/rl_glue --pv`
FILENAME=RL-Glue-Windows-Binary-$VERSION.zip
THEFILE=./dist/$FILENAME
python ../googlecode_upload.py -s "Mac OS X RL-Glue Core Project and C/C++ Codec Disk Image $VERSION" -p rl-glue-ext --labels=Type-Executable,OpSys-Windows  $THEFILE
