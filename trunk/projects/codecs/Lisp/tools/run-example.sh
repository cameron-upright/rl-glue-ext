#!/bin/bash
#
# Runs a specified example with a selected Lisp implementation
# and prints the result to the standard output.
#
# $Revision$
# $Date$

if [ ${#} -ne 2 ]; then
    echo "Usage: ${0} <lisp-implementation> <example>"
    echo "Example: ${0} sbcl mines-sarsa"
    exit -1
fi

tooldir="`dirname ${0}`"
lispimpl="${1}"
example="${2}"

###############################################################################

LISPCFGFILE="${tooldir}/config/lisp-${lispimpl}"
if [ ! -e "${LISPCFGFILE}" ]; then
    echo "Not supported lisp implementation: ${lispimpl}!"
    exit -2
fi
source "${LISPCFGFILE}"
source "${tooldir}/config/init.sh"

###############################################################################

${LISP} <<- EOF
  `lisp-init`
  (asdf:oos 'asdf:compile-op :rl-glue-examples :verbose nil)
  `lisp-quit`
EOF

###############################################################################

rl_glue &

{
${LISP} <<- EOF
  `lisp-init`
  (asdf:oos 'asdf:load-op :rl-glue-examples :verbose nil)
  (rl-glue-${example}:start-agent)
  `lisp-quit`
EOF
} &

{
${LISP} <<- EOF
  `lisp-init`
  (asdf:oos 'asdf:load-op :rl-glue-examples :verbose nil)
  (rl-glue-${example}:start-environment)
  `lisp-quit`
EOF
} &

${LISP} <<- EOF
  `lisp-init`
  (asdf:oos 'asdf:load-op :rl-glue-examples :verbose nil)
  (rl-glue-${example}:start-experiment)
  `lisp-quit`
EOF

exit 0

