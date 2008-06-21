function RL_set_state(S)
    global rlglue__struct;
    rlglue__struct.env_set_state(S);
end