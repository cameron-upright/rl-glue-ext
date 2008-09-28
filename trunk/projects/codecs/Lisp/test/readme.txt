
                         ------------------------------
                         Testing the RL-Glue Lisp codec
                         ------------------------------

    1. About testing in general
    2. Test descriptions

-------------------------------------------------------------------------------

1. About testing in general

    Testing are helped by makefiles and shell scripts, but they only support
    lisp implementations (on the same way) as was described in the examples
    part of the install document.

    Compiling the tests.

        cd tests
        make

    Running a test (e.g. test-1)

        cd test-1
        chmod +x test-1.sh (if necessary)
        ./test-1.sh

    In case of not supported lisp implmentation, the tests can be run manually
    from the lisp interpreter.

    Cleaning the test suite.

        cd tests
        make clean

2. Test descriptions

    These descriptions can be found in the own directories of the tests.

