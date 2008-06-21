function RL_set_random_seed(RS)
    global rlglue__struct;
    rlglue__struct.env_set_random_seed(RS);
end