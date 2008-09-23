function theEnvironment=test_empty_environment_construct()
    theEnvironment.env_init=@test_empty_environment_init;
    theEnvironment.env_start=@test_empty_environment_start;
    theEnvironment.env_step=@test_empty_environment_step;
    theEnvironment.env_set_state=@test_empty_environment_set_state;
    theEnvironment.env_get_state=@test_empty_environment_get_state;
    theEnvironment.env_set_random_seed=@test_empty_environment_set_random_seed;
    theEnvironment.env_get_random_seed=@test_empty_environment_get_random_seed;
    theEnvironment.env_cleanup=@test_empty_environment_cleanup;
    theEnvironment.env_message=@test_empty_environment_message;
end