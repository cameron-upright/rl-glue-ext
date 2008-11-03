function theEnvironment=test_seeds_environment()
    theEnvironment.env_init=@test_seeds_environment_init;
    theEnvironment.env_start=@test_seeds_environment_start;
    theEnvironment.env_step=@test_seeds_environment_step;
    theEnvironment.env_load_state=@test_seeds_environment_load_state;
    theEnvironment.env_save_state=@test_seeds_environment_save_state;
    theEnvironment.env_load_random_seed=@test_seeds_environment_load_random_seed;
    theEnvironment.env_save_random_seed=@test_seeds_environment_save_random_seed;
    theEnvironment.env_cleanup=@test_seeds_environment_cleanup;
    theEnvironment.env_message=@test_seeds_environment_message;
end

function taskSpec=test_seeds_environment_init()
    global test_seeds_environment_struct;

	test_seeds_environment_struct.seed=org.rlcommunity.rlglue.codec.types.Random_seed_key();
	test_seeds_environment_struct.state=org.rlcommunity.rlglue.codec.types.State_key();
	

	test_seeds_environment_struct.emptyObservation=org.rlcommunity.rlglue.codec.types.Observation();
	taskSpec='';
end    

function theObservation=test_seeds_environment_start()
    global test_seeds_environment_struct;

	theObservation=test_seeds_environment_struct.emptyObservation;
end

function rewardObservation=test_seeds_environment_step(theAction)
    global test_seeds_environment_struct;

	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation();
	rewardObservation.o=test_seeds_environment_struct.emptyObservation;
end

function returnMessage=test_seeds_environment_message(theMessageJavaObject)
	returnMessage='';
end

function test_seeds_environment_cleanup()
end

function test_seeds_environment_load_seed(theState)
	global test_seeds_environment_struct;
	test_seeds_environment_struct.state=theState;
end

function test_seeds_environment_load_random_seed(randomSeed)
	global test_seeds_environment_struct;
	test_seeds_environment_struct.seed=randomSeed;
end
function theState=test_seeds_environment_save_state()
	    global test_seeds_environment_struct;

	theState=test_seeds_environment_struct.state;
end

function randomSeed=test_seeds_environment_save_random_seed()
    global test_seeds_environment_struct;

randomSeed=test_seeds_environment_struct.seed;
end