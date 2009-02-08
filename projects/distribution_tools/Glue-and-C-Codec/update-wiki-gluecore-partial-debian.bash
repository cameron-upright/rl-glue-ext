#!/bin/bash

#Update the Debian GlueCore .deb wiki partial
VERSION=?
FILENAME=?
python filesubstitution.py $VERSION $FILENAME wiki/templates/RLGlueCore.wiki.debian.template wiki/current/RLGlueCore.wiki.debian
