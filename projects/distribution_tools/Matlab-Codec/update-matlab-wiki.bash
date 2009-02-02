#!/bin/bash
python substitute-matlab-strings.py
cp Matlab.wiki ../wiki/
cd ../wiki
svn commit Matlab.wiki -m "Automated update of Matlab wiki page."