#! /bin/sh

# deb=>rpm to Upstream Script for RL-Glue
# Version: v1.1 - modified 23-2-2009
# Author: Iordanis Daroglou <idaroglo@csd.auth.gr>
# Supported build arches: i386, amd64
# parameters:
# the upstream version number we are updating FROM ($1)
# the latest upstream version we are updating TO ($2)

BASEDIR=$(pwd)
# DEBBASE are the place where the respective debbaseX.tar.gz's are extracted
DEBBASE_32_DIR=$BASEDIR/debian/32bit
DEBBASE_64_DIR=$BASEDIR/debian/64bit

VERSION=$1
# convienience variables for the names of the dirs
# that hold the sources for each version
NEW_UPV=rl-glue-$VERSION

# target dir(s), where the debs & rpms end up
DEB_DIST=dist
RPM_DIST=dist

# latest upstream distribution
CURRENT_GLUE_TAR=rlglue-$VERSION.tar.gz
CURRENT_GLUE_DIST=dist/$CURRENT_GLUE_TAR

# prepare the updated 32-bit package debian sources 
#cp $CURRENT_GLUE_DIST $DEBBASE_32_DIR
#cd $DEBBASE_32_DIR/$OLD_VER
#uupdate -u $DEBBASE_32_DIR/$CURRENT_GLUE_TAR
# build the package, check with lintian (create a log)
# and place the deb in target dir
#cd ../$NEW_VER
#dpkg-buildpackage -rfakeroot -uc -us
#lintian ../rl-glue_$2-1_i386.deb > lintian32
#fakeroot alien -r rl-glue_$2-1_i386.deb
#cp ../rl-glue_$2-1_i386.deb $DEB_DIST

# prepare the updated 64-bit package debian sources
cp $CURRENT_GLUE_DIST $DEBBASE_64_DIR
cd $DEBBASE_64_DIR/old
uupdate -u $DEBBASE_64_DIR/$CURRENT_GLUE_TAR
# build the package, check with lintian (create a log)
# and place the deb in target dir
cd ../$NEW_VER
dpkg-buildpackage -rfakeroot -uc -us
lintian ../rl-glue_$VERSION-1_amd64.deb > lintian64
fakeroot alien -r rl-glue_$VERSION-1_amd64.deb
cp ../rl-glue_$VERSION-1_amd64.deb $DEB_DIST

# copy rpms to distribution target dir..
# this needs finetuning - will have to adjust to rpm
# filename conventions (to issue copy on specific files)
cp ../*.rpm $RPM_DIST

# we should find a way to automatically update the Debian changelog
# not much important, though

