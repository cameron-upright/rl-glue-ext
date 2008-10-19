#!/bin/sh

function make_asdf_package {
    local system=$1
    echo -n "Creating package $system.tar.gz ... "
    tar -zcf ../$system.tar.gz --exclude '.svn/*' --exclude '.svn' $system
    if [ $? = 0 ]; then
        echo "done"
    else
        echo "failed ($?)"
    fi
}

if [ $# != 1 ]; then
    echo "Usage: $0 <asdf system name>"
    exit 1
fi

cd src
make_asdf_package $1
cd ..

exit 0

