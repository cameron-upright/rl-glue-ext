#!/bin/bash
#This is a do-script because its not meant to be called by any others.  It's top level (for this dir at least)
#This will download and build RL-Glue and the Codec, build the Mac disk image, and upload it.
bash ./download-build-glue.bash
bash ./create-glue-dist.bash
#This could be 32 or 64 
sh ./create-debian-package.bash 
bash ./update-wiki-gluecore-partial-deb32.bash
bash ./update-wiki-gluecore-partial-rpm32.bash
bash ./upload-deb32.bash
bash ./upload-rpm32.bash
bash ./do-finalize-wikis.bash
