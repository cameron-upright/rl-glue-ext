#!/bin/sh

function make_asdf_package {
    local package=$1
    tar -zcf $package.tar.gz --exclude '.svn/*' --exclude '.svn' $package
}

make_asdf_package 'rl-glue-clcdc'
make_asdf_package 'rl-glue-utils'

