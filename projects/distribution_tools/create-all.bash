#!/bin/bash
MATLAB_VERSION=1.0
cd Glue-and-C-Codec
bash create-all-dist.bash
cd ..

cd Java-Codec
bash download-java.bash
cd ..

cd Matlab-Codec
bash download-matlab.bash $MATLAB_VERSION
cd ..

