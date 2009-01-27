
basedir="`echo ${PWD}/${tooldir} | grep -oP '^.*/rl-glue-ext/.*/Lisp'`"
echo "base directory : ${basedir}"

function lisp-init
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

function lisp-quit
{
cat <<- EOF
    #+allegro
    (exit)
    #-allegro
    (quit)
EOF
}

