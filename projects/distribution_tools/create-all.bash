#!/bin/bash
MATLAB_VERSION=1.0-RC-FINAL-1
C_CODEC_VERSION=2.0-RC-FINAL-1
LISP_VERSION=1.0-RC-FINAL-1
cd Glue-and-C-Codec
bash create-all-dist.bash $C_CODEC_VERSION
cd ..

cd Java-Codec
bash download-java.bash
cd ..

cd Matlab-Codec
bash download-matlab.bash $MATLAB_VERSION
cd ..

cd Python-Codec
bash download-python.bash
cd ..

cd Lisp-Codec
bash download-lisp.bash $LISP_VERSION
cd ..
