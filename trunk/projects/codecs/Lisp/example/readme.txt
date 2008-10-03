                    -----------------------------------
                    Examples for the RL-Glue Lisp Codec
                    -----------------------------------

    1. Using the examples
    2. Description of the modules

-------------------------------------------------------------------------------

1. Using the examples

    There are makefiles for the examples, which makes possible to try the
    codec without installation (but all the required libraries should be
    preinstalled to the system). The used lisp implementation (if supported)
    is autodetected by the makefiles. If more of them are found the order is
    SBCL, CMUCL, CLISP, OpenMCL. If the default is not good, it can be
    overridden by the LISP_IMPL environment variable. Its supported values are
    'sbcl', 'cmucl', 'clisp' and 'openmcl'.

    Because CLISP does not provide the 'require' function, the path to
    the asdf.lisp file and the asd system definitions have to be specified
    (in a space separated way) by the LISP_ASDF_DIRS environment varaible.
    Because it has a default value (can be checked in makefile.rl-glue),
    most unix user can ignore its setting.

    So usually the following is enough to compile the examples.

        cd examples
        make
        
    Cleaning the example directories.

        cd examples
        make clean

2. Descriptions of the modules

    A readme document about the agent, environment and experiment examples
    can be found in their directories.

