function RL_cleanup()
    global rlglue__struct;
    rlglue__struct.env_cleanup();
    rlglue__struct.agent_cleanup();
end