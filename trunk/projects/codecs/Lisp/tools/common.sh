#
# Common initializations and functions of the tools.
#
# $Revision$
# $Date$

logdir="${tooldir}/log"
basedir="`echo ${PWD}/${tooldir} | grep -oP '^.*/rl-glue-ext/.*/Lisp'`"

###############################################################################

function load_lisp_config
{
    lispimpl=${1}
    LISPCFGFILE="${tooldir}/config/lisp-${lispimpl}"
    if [ ! -e "${LISPCFGFILE}" ]; then
        echo "Not supported lisp implementation: ${lispimpl}!"
        exit -2
    fi
    source "${LISPCFGFILE}"
}

function load_functional_test_config
{
    testname=${1}
    source "${tooldir}/../test/functional/${testname}/config"
}

###############################################################################

function check_test_component_name
{
    compname=${1}
    case ${compname} in
        "agent" ) ;;
        "environment" ) ;;
        "experiment" ) ;;
        * ) echo "Invalid test component: ${compname}!"; exit -1 ;;
    esac
}

###############################################################################

function lisp_init
{
cat <<- EOF
  (load #p"${tooldir}/config/init")
  (progn
    (dolist (relpath (list #p"${basedir}/examples/"
                           #p"${basedir}/src/rl-glue-codec/"
                           #p"${basedir}/src/rl-glue-utils/"
                           #p"${basedir}/test/functional/"
                           #p"${basedir}/test/unit/rl-glue-codec/"
                           #p"${basedir}/test/unit/rl-glue-utils/"))
      (push relpath asdf:*central-registry*)))
EOF
}

function lisp_quit
{
cat <<- EOF
    #+allegro
    (exit)
    #-allegro
    (quit)
EOF
}

###############################################################################

echo "Lisp codec directory : ${basedir}"

