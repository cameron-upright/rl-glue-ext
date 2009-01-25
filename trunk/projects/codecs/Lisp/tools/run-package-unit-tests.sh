#!/bin/bash
#
# Runs the unit tests of the specified package with a selected Lisp
# implementation and prints the result to the standard output.
#
# $Revision$
# $Date$

if [ ${#} -ne 2 ]; then
    echo "Usage: ${0} <lisp-implementation> <package>"
    echo "Example: ${0} sbcl rl-glue-utils"
    exit -1
fi

tooldir="`dirname ${0}`"
lispimpl="${1}"
package="${2}"

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
  (handler-bind ((condition #'(lambda (condition)
                                (continue))))
    (asdf:oos 'asdf:load-op :${package}-tests :verbose nil))
  (fiveam:run! '${package}::main-suite)
  `lisp-quit`
EOF

exit 0

