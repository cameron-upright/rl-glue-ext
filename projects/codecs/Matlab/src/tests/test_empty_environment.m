function theEnvironment=test_empty_environment()
    theEnvironment.env_init=@test_empty_environment_init;
    theEnvironment.env_start=@test_empty_environment_start;
    theEnvironment.env_step=@test_empty_environment_step;
    theEnvironment.env_cleanup=@test_empty_environment_cleanup;
    theEnvironment.env_message=@test_empty_environment_message;
end

function taskSpec=test_empty_environment_init()
    global test_empty_environment_struct;

	test_empty_environment_struct.whichEpisode=0;
	test_empty_environment_struct.emptyObservation=org.rlcommunity.rlglue.codec.types.Observation();
	test_empty_environment_struct.nonEmptyObservation=org.rlcommunity.rlglue.codec.types.Observation();
	test_empty_environment_struct.nonEmptyObservation.intArray=[0 1];
	test_empty_environment_struct.nonEmptyObservation.doubleArray=[0/4 1/4 2/4 3/4];
	test_empty_environment_struct.nonEmptyObservation.charArray=['abcde'];

	taskSpec='';
end    


function theObservation=test_empty_environment_start()
    global test_empty_environment_struct;

	test_empty_environment_struct.whichEpisode=test_empty_environment_struct.whichEpisode+1;

	if mod(test_empty_environment_struct.whichEpisode,2) == 0
		theObservation=test_empty_environment_struct.emptyObservation;
	else
		theObservation=test_empty_environment_struct.nonEmptyObservation;
	end
end


function rewardObservation=test_empty_environment_step(theAction)
    global test_empty_environment_struct;

	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal();

	if mod(test_empty_environment_struct.whichEpisode,2) == 0
		rewardObservation.o=test_empty_environment_struct.emptyObservation;
	else
		rewardObservation.o=test_empty_environment_struct.nonEmptyObservation;
	end
end

function returnMessage=test_empty_environment_message(theMessageJavaObject)
	returnMessage='';
end

function test_empty_environment_cleanup()
end
