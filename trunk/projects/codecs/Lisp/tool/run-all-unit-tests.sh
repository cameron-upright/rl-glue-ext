#!/bin/bash
#
# Runs all the unit tests with the selected lisp implementation.
# If the lisp implementation is not specified, the test are run with all
# the supported and available ones. It prints the result and a summary
# to the standard output and into a log file as well.
#
# $Revision$
# $Date$

if [ ${#} -gt 1 ]; then
echo "Usage: ${0} [<lisp-implementation>]"
    exit -1
fi

tooldir="`dirname ${0}`"

lispimpl=""
if [ ${#} -eq 1 ]; then
    lispimpl="${1}"
fi

###############################################################################

LOGFILE="${tooldir}/log/run-all-unit-tests.log"
echo -en "`date`\n\n" > ${LOGFILE}

{
    for c in `ls "${tooldir}/config/lisp-${lispimpl}"*`; do
        l=`basename ${c} | cut -d'-' -f2-`
        echo -e "\nFT> ---- CONFIG : ${l}\n"
        source "${c}"

        if [ -z "`which ${LISPBIN} 2>/dev/null`" ]; then
            echo "FT> Lisp binary is not available: ${LISPBIN}."
            continue
        fi

        for p in `ls -d "${tooldir}/../src/"*`; do
            if [ -d "${p}" ]; then
                echo " ---- PACKAGE : ${p}"
                ${tooldir}/run-package-unit-tests.sh ${l} `basename ${p}`
            fi
        done
    done
} 2>&1 | tee -a ${LOGFILE}

exit 0

