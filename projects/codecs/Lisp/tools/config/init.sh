
basedir="`realpath ${tooldir}/../`"

function lisp-init
{
cat <<- EOF
    (load #p"${tooldir}/config/init")
    (progn
      (dolist (relpath (list #p"${basedir}/example/"
                             #p"${basedir}/src/rl-glue-codec/"
                             #p"${basedir}/src/rl-glue-utils/"
                             #p"${basedir}/test/functional/"
                             #p"${basedir}/test/unit/rl-glue-codec/"
                             #p"${basedir}/test/unit/rl-glue-utils/"))
        (push relpath asdf:*central-registry*))
      'initialization-done)
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

