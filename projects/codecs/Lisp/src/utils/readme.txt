
                    --------------------------------
                        RL-Glue Utilities (Lisp)
                    --------------------------------

    1. Package of RL-Glue utilities
    2. Utility descriptions
        2.1 Parser for the task specification language

-------------------------------------------------------------------------------

1. Package of RL-Glue utilities

    The utilities are organized under the rl-glue-utils package.
    All of the utilities (rl-glue-utils package) can be loaded by

        (asdf:operate 'asdf:load-op 'rl-glue-utils)

    Required libraries are described on utility basis. Their summary
    can be checked in the rl-glue-utils.asd file.

2. Utility descriptions

    2.1 Parser for the task specification language

        Parser for the task specification language described on
        http://rlai.cs.ualberta.ca/RLBB/TaskSpecification.html.

        Required library : cl-ppcre [http://www.cliki.net/CL-PPCRE]

