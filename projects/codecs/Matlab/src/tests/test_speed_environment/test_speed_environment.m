%Return an instance of this environment
function theEnvironment=test_speed_environment()
    theEnvironment=test_speed_environment_construct();
end


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

function taskSpec=test_speed_environment_init()
    global test_speed_environment_struct;

	test_speed_environment_struct.whichEpisode=0;
	test_speed_environment_struct.stepCount=0;
	taskSpec='';
end    
   

function theObservation=test_speed_environment_start()
    global test_speed_environment_struct;

	test_speed_environment_struct.whichEpisode=test_speed_environment_struct.whichEpisode+1;
	test_speed_environment_struct.stepCount=0;
	test_speed_environment_struct.o=org.rlcommunity.rlglue.codec.types.Observation;
	theObservation=test_speed_environment_struct.o;
end

function rewardObservation=test_speed_environment_step(theAction)
    global test_speed_environment_struct;
	test_speed_environment_struct.stepCount=test_speed_environment_struct.stepCount+1;

	        org.rlcommunity.rlglue.codec.tests.TestUtility.clean_abstract_type(test_speed_environment_struct.o);


	if mod(test_speed_environment_struct.whichEpisode,2) == 0
        org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_ints_in_abstract_type(test_speed_environment_struct.o, 50000);
        org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_doubles_in_abstract_type(test_speed_environment_struct.o, 50000);
		terminal=0;
		if(test_speed_environment_struct.stepCount==200)
			terminal=1;
        end
        rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation(1.0,test_speed_environment_struct.o,terminal);	
	else
        org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_ints_in_abstract_type(test_speed_environment_struct.o, 5);
        org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_doubles_in_abstract_type(test_speed_environment_struct.o,5);
		terminal=0;
		if(test_speed_environment_struct.stepCount==5000)
			terminal=1;
		end
		rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation(1.0,test_speed_environment_struct.o,terminal);	
	end
end
	

function theState=test_speed_environment_get_state()
	theState=org.rlcommunity.rlglue.codec.types.State_key();
end

function randomSeed=test_speed_environment_get_random_seed()
	randomSeed=org.rlcommunity.rlglue.codec.types.Random_seed_key();
end

function test_speed_environment_cleanup()
end

function test_speed_environment_set_random_seed(randomSeed)
end

function test_speed_environment_set_state(stateKey)
end


function returnMessage=test_speed_environment_message(theMessageJavaObject)
	returnMessage='';
end