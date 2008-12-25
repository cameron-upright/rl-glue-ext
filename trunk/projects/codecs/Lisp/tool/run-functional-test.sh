#!/bin/bash
#
# Runs a specified functional test with a selected Lisp implementation
# and prints the result to the standard output.
#
# $Revision$
# $Date$

if [ ${#} -ne 2 ]; then
    echo "Usage: ${0} <lisp-implementation> <testcase-name>"
    echo "Example: ${0} sbcl test-empty"
    exit -1
fi

tooldir="`dirname ${0}`"
lispimpl="${1}"
testname="${2}"

###############################################################################

LISPCFGFILE="${tooldir}/config/lisp-${lispimpl}"
if [ ! -e "${LISPCFGFILE}" ]; then
    echo "Not supported lisp implementation: ${lispimpl}!"
    exit -2
fi
source "${LISPCFGFILE}"
source "${tooldir}/../test/functional/${testname}/config"

###############################################################################

${LISP} ${EVAL} "(asdf:oos 'asdf:compile-op :rl-glue-tests :verbose nil)" \
        ${EVAL} "(quit)"

###############################################################################

rl_glue &

${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :rl-glue-tests :verbose nil)" \
        ${EVAL} "(rl-glue-tests:start-${AGENT})" \
        ${EVAL} "(quit)" &

${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :rl-glue-tests :verbose nil)" \
        ${EVAL} "(rl-glue-tests:start-${ENVIRONMENT})" \
        ${EVAL} "(quit)" &

${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :rl-glue-tests :verbose nil)" \
        ${EVAL} "(rl-glue-tests:start-${EXPERIMENT})" \
        ${EVAL} "(quit)"

exit 0

