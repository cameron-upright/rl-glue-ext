
                     -------------------------------------
                          RL-Glue Agent Client (Lisp)
                     -------------------------------------

    1. How to write your own agent?
    2. How to run your agent?

-------------------------------------------------------------------------------

1. How to write your own agent?

    First, create your agent class, e.g.

        (defclass my-agent (agent) ...)

    Second, implement the following methods.

        (defmethod agent-init ((agent my-agent) task-spec) ...)
        (defmethod agent-start ((agent my-agent) first-observation) ...)
        (defmethod agent-step ((agent my-agent) reward observation) ...)
        (defmethod agent-end ((agent my-agent) reward) ...)
        (defmethod agent-cleanup ((agent my-agent)) ...)
        (defmethod agent-message ((agent my-agent) input-message) ...)

    A detailed description of the methods can be obtained by

        (documentation #'<method name> 'function)

2. How to run your agent?

    Call the run-agent function with your agent, e.g.

        (run-agent (make-instance 'my-agent)
                   :host "192.168.1.1"
                   :port 4096
                   :retry-timeout 10)

    which tries to connect your agent to an RL-Glue listening on
    192.168.1.1 and port 4096, waiting 10 seconds between the trials.
    One can check its detailed description by

        (documentation #'run-agent 'function)

