function S=RL_get_state()
    global rlglue__struct;
    S=rlglue__struct.env_get_state();
end