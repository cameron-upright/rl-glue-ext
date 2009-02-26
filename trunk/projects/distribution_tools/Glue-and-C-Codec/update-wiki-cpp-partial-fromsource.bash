#!/bin/bash
#Update the GlueCore deb package
if [ -z $1 ]
then
  CODECVERSION=UNKNOWN
  echo "You didn't pass a version for the codec so using UNKNOWN"
else
  CODECVERSION=$1
fi

CODECNAME=c-codec-$CODECVERSION
CODECUPLOADFILE=$CODECNAME.tar.gz
CODECZIP=dist/$CODECUPLOADFILE
#Upload to Google Code
#python ../googlecode_upload.py -s "RL-Glue C/C++ Codec $CODECVERSION" -p rl-glue-ext --labels=Type-Installer,OpSys-All,Language-C,Audience-User,Audience-Dev $CODECZIP

# Update the Wiki

# Update the Wiki
python filesubstitution.py $CODECVERSION $CODECUPLOADFILE wiki/templates/CandCPP.wiki.fromsource.template wiki/current/CandCPP.wiki.fromsource

