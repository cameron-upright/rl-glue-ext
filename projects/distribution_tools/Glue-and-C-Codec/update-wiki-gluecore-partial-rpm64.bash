#!/bin/bash
#Update the RPM Wiki Template
VERSION=`./install_root/usr/local/bin/rl_glue --pv`
FILENAME=rl-glue-${VERSION}-2.x86_64.rpm
python filesubstitution.py $VERSION $FILENAME wiki/templates/RLGlueCore.wiki.rpm64.template wiki/current/RLGlueCore.wiki.rpm64


