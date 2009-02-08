#!/bin/bash
#Upload the C/C++ codec tarball to Google Code

if [ -z $1 ]
then
  CODECVERSION=UNKNOWN
  echo "You didn't pass a version for the codec so using UNKNOWN"
else
  CODECVERSION=$1
fi

CODECNAME=c-codec-$CODECVERSION
CODECZIP=dist/$CODECNAME.tar.gz

python ../googlecode_upload.py -s "RL-Glue C/C++ Codec $CODECVERSION" -p rl-glue-ext --labels=Type-Installer,OpSys-All,Language-C,Audience-User,Audience-Dev $CODECZIP
