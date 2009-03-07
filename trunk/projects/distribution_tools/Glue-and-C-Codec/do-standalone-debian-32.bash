#!/bin/bash
#This is a do-script because its not meant to be called by any others.  It's top level (for this dir at least)
bash ./download-build-glue.bash
bash ./create-glue-dist.bash
#This could be 32 or 64 
sh ./create-debian-package.bash 
bash ./update-wiki-gluecore-partial-deb32.bash
bash ./update-wiki-gluecore-partial-rpm32.bash
bash ./upload-deb32.bash
bash ./upload-rpm32.bash
bash ./do-finalize-wikis.bash
