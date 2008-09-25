
                        --------------------------
                            RL-Glue Lisp Codec
                        --------------------------

    1. Usage of the codec
    2. Symbols and packages
    3. Makefile

-------------------------------------------------------------------------------

1. Usage of the codec

    We recommend to use ASDF (http://www.cliki.net/asdf)
    to load the rl-glue package.

        (asdf:operate 'asdf:load-op 'rl-glue)

    If one needs the utilities as well, they have to be loaded separately.

        (asdf:operate 'asdf:load-op 'rl-glue-utils)

    Detailed descriptions about the interfaces can be found in the agent,
    environment and experiment directories in the readme files.

2. Symbols and packages

    The RL-Glue interface has an own package named rl-glue. One can import
    all the symbols into one's package by the :use directive, e.g.

        (defpackage #:my-package (:use #:rl-glue ...) ...)

    and use the rl-glue symbols without any package-qualification,
    or one can use the proper package-qualified symbol names prefixed
    by rl-glue, e.g.

        (defclass my-agent (rl-glue:agent) ...)
        (defmethod rl-glue:env-init ((env my-env)) ...)

3. Makefile

    There is a special makefile called rl-glue.makefile, which can be used
    by other makefiles to support a uniform Lisp scripting functionalities
    to RL-Glue agent, environment and experiment clients.

    This makefile supports SBCL, CMUCL and CLISP at the moment.

    User-definable variables in the makefile are
    LISP : lisp executable (e.g. sbcl)
    LISP_PATH : paths to the asdf.lisp file and
                to the asdf systems' directory
                (only needed for CLISP)

