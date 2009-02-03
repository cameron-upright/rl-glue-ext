#!/bin/bash
if [ -z $1 ]
then
  CODECVERSION=UNKNOWN
  echo "You didn't pass a version for the codec so using UNKNOWN"
else
  CODECVERSION=$1
fi

bash ./download-build-glue.bash
bash ./download-build-codec.bash
bash ./create-glue-dist.bash
bash ./create-codec-dist.bash
bash ./prepare-mac-package.bash
bash ./upload-files.bash $CODECVERSION
bash ./cleanup.bash