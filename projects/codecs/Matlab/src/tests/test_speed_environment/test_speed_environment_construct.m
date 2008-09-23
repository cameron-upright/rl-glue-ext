function theEnvironment=test_speed_environment_construct()
    theEnvironment.env_init=@test_speed_environment_init;
    theEnvironment.env_start=@test_speed_environment_start;
    theEnvironment.env_step=@test_speed_environment_step;
    theEnvironment.env_set_state=@test_speed_environment_set_state;
    theEnvironment.env_get_state=@test_speed_environment_get_state;
    theEnvironment.env_set_random_seed=@test_speed_environment_set_random_seed;
    theEnvironment.env_get_random_seed=@test_speed_environment_get_random_seed;
    theEnvironment.env_cleanup=@test_speed_environment_cleanup;
    theEnvironment.env_message=@test_speed_environment_message;
end