
                        --------------------------
                            RL-Glue Lisp Codec
                        --------------------------

    1. Usage of the codec
    2. Symbols and packages

-------------------------------------------------------------------------------

1. Usage of the codec

    We recommend to use ASDF (http://www.cliki.net/asdf)
    to load the rl-glue-clcdc package.

        (asdf:operate 'asdf:load-op 'rl-glue-clcdc)

    Detailed descriptions about the interfaces can be found in the agent,
    environment and experiment directories in the readme files.

2. Symbols and packages

    The RL-Glue interface has an own package named rl-glue-clcdc. One can
    import all the symbols into one's package by the :use directive, e.g.

        (defpackage #:my-package (:use #:rl-glue ...) ...)

    and use the rl-glue-clcdc symbols without any package-qualification,
    or one can use the proper package-qualified symbol names prefixed
    by rl-glue-clcdc, e.g.

        (defclass my-agent (rl-glue-clcdc:agent) ...)
        (defmethod rl-glue-clcdc:env-init ((env my-env)) ...)

