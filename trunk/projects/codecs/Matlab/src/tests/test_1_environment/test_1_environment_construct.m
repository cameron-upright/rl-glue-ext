function theEnvironment=test_1_environment_construct()
    theEnvironment.env_init=@test_1_environment_init;
    theEnvironment.env_start=@test_1_environment_start;
    theEnvironment.env_step=@test_1_environment_step;
    theEnvironment.env_set_state=@test_1_environment_set_state;
    theEnvironment.env_get_state=@test_1_environment_get_state;
    theEnvironment.env_set_random_seed=@test_1_environment_set_random_seed;
    theEnvironment.env_get_random_seed=@test_1_environment_get_random_seed;
    theEnvironment.env_cleanup=@test_1_environment_cleanup;
    theEnvironment.env_message=@test_1_environment_message;
end