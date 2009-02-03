#!/bin/bash
if [ -z $1 ]
then
  CODECVERSION=UNKNOWN
  echo "You didn't pass a version for the codec so using UNKNOWN"
else
  CODECVERSION=$1
fi

GLUEVERSION=`./install_root/usr/local/bin/rl_glue --pv`

GLUENAME=rlglue-$GLUEVERSION
CODECNAME=c-codec-$CODECVERSION
MACNAME=RL-Glue-$GLUEVERSION-and-C-Codec.dmg

GLUEZIP=dist/$GLUENAME.tar.gz
CODECZIP=dist/$CODECNAME.tar.gz
MACDMG=dist/$MACNAME

#Upload to Google Code
python ../googlecode_upload.py -s "RL-Glue Core Installation $GLUEVERSION" -p rl-glue-ext --labels=Type-Installer,OpSys-All,Language-C,Audience-User,Audience-Dev $GLUEZIP
#python ../googlecode_upload.py -s "RL-Glue Core Installation $GLUEVERSION" -p rl-glue --labels=Type-Installer,OpSys-All $GLUEZIP
python ../googlecode_upload.py -s "Mac OS X RL-Glue Core Project and C/C++ Codec Disk Image $GLUEVERSION" -p rl-glue-ext --labels=Type-Installer,OpSys-OSX  $MACDMG
python ../googlecode_upload.py -s "RL-Glue C/C++ Codec $CODECVERSION" -p rl-glue-ext --labels=Type-Installer,OpSys-All,Language-C,Audience-User,Audience-Dev $CODECZIP

# Update the Wiki
python substitute-glue-strings.py $GLUEVERSION $GLUENAME $MACNAME
python substitute-codec-strings.py $CODECVERSION $CODECNAME $MACNAME
cp RLGlueCore.wiki ../wiki/core.new
cp CandCPP.wiki ../wiki/candcpp.new
cd ../wiki
svn up
mv core.new RLGlueCore.wiki
mv candcpp.new CandCPP.wiki
svn commit RLGlueCore.wiki CandCPP.wiki -m "Automated update of RLGlueCore and CandCPP wiki pages."
