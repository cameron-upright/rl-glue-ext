#!/bin/sh
#
# Runs a specified cross functional test with two C codec components and
# one Lisp codec component. The result is printed to the standard output.
#
# $Revision$
# $Date$

if [ ${#} -ne 3 ]; then
    echo "Usage: ${0} <lisp-implementation> <testcase-name> <lisp-component>"
    echo "Example: ${0} sbcl test-empty environment"
    exit -1
fi

tooldir="`dirname ${0}`"
source "${tooldir}/common.sh"

lispimpl="${1}"
testname="${2}"
lispcomp="${3}"

check_test_component_name ${lispcomp}

###############################################################################

load_lisp_config ${lispimpl}
load_functional_test_config ${testname}

###############################################################################

rl_glue &

if [ ${lispcomp} = "agent" ]; then
{
${LISP} <<- EOF
  `lisp_init`
  (asdf:oos 'asdf:load-op :rl-glue-tests :verbose nil)
  (rl-glue-tests:start-${AGENT})
  `lisp_quit`
EOF
} &
else
    cd ${basedir}/../C/tests/ &&
    make ${C_AGENT} &&
    ${basedir}/../C/tests/${C_AGENT} &
fi

if [ ${lispcomp} = "environment" ]; then
{
${LISP} <<- EOF
  `lisp_init`
  (asdf:oos 'asdf:load-op :rl-glue-tests :verbose nil)
  (rl-glue-tests:start-${ENVIRONMENT})
  `lisp_quit`
EOF
} &
else
    cd ${basedir}/../C/tests/ &&
    make ${C_ENVIRONMENT} &&
    ${basedir}/../C/tests/${C_ENVIRONMENT} &
fi

if [ ${lispcomp} = "experiment" ]; then
{
${LISP} <<- EOF
  `lisp_init`
  (asdf:oos 'asdf:load-op :rl-glue-tests :verbose nil)
  (rl-glue-tests:start-${EXPERIMENT})
  `lisp_quit`
EOF
}
else
    cd ${basedir}/../C/tests/ &&
    make ${C_EXPERIMENT} &&
    ${basedir}/../C/tests/${C_EXPERIMENT}
fi

exit 0

