
                ----------------------------------------
                    RL-Glue Experiment Client (Lisp)
                ----------------------------------------

    1. How to write your own experiment?

-------------------------------------------------------------------------------

1. How to write your own experiment?

    First, create your experiment class, e.g.

        (defclass my-exp (experiment) ...)

    Second, implement your experiment. For this RL-Glue provides
    client functions which hides the necessary buffer handling
    and network operation. These functions are

        rl-init
        rl-start
        rl-step
        rl-cleanup
        rl-return
        rl-num-steps
        rl-num-episodes
        rl-episode
        rl-get-state
        rl-set-state
        rl-get-random-seed
        rl-set-random-seed
        rl-agent-message
        rl-env-message

    A detailed description of these functions can be obtained by

        (documentation #'<function name> 'function)

