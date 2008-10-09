#!/bin/sh

function make_asdf_package {
    local package=$1
    echo -n "Creating package $package.tar.gz ... "
    tar -zcf ../$package.tar.gz --exclude '.svn/*' --exclude '.svn' $package
    echo "done"
}

cd src
make_asdf_package 'rl-glue-codec'
make_asdf_package 'rl-glue-utils'
cd ..

