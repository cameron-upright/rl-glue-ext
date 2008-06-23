function RS=RL_get_random_seed()
    global rlglue__struct;
    RS=rlglue__struct.env_get_random_seed();
end