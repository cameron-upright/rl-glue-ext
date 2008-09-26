
               ------------------------------------------
                    RL-Glue Environment Client (Lisp)
               ------------------------------------------

    1. How to write your own environment?
    2. How to run your environment?

-------------------------------------------------------------------------------

1. How to write your own environment?

    First, create your environment class, e.g.

        (defclass my-env (environment) ...)

    Second, implement the following methods.

        (defmethod env-init ((env my-env)) ...)
        (defmethod env-start ((env my-env)) ...)
        (defmethod env-step ((env my-env) action) ...)
        (defmethod env-get-state ((env my-env)) ...)
        (defmethod env-set-state ((env my-env) state-key) ...)
        (defmethod env-get-random-seed ((env my-env)) ...)
        (defmethod env-set-random-seed ((env my-env) random-seed-key) ...)
        (defmethod env-cleanup ((env my-env)) ...)
        (defmethod env-message ((env my-env) input-message) ...)

    A detailed description of the methods can be obtained by

        (documentation #'<method name> 'function)

2. How to run your environment?

    Call the run-env function with your environment, e.g.

        (run-env (make-instance 'my-env)
                 :host "192.168.1.1"
                 :port 4096
                 :retry-timeout 10)

    which tries to connect your environment to an RL-Glue listening on
    192.168.1.1 and port 4096, waiting 10 seconds between the trials.
    One can check its detailed description by

        (documentation #'run-env 'function)

