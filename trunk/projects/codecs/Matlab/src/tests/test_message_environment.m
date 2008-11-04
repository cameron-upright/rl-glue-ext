function theEnvironment=test_message_environment()
    theEnvironment.env_init=@test_message_environment_init;
    theEnvironment.env_start=@test_message_environment_start;
    theEnvironment.env_step=@test_message_environment_step;
    theEnvironment.env_cleanup=@test_message_environment_cleanup;
    theEnvironment.env_message=@test_message_environment_message;
end

function taskSpec=test_message_environment_init()
    global test_message_environment_struct;

	test_message_environment_struct.emptyObservation=org.rlcommunity.rlglue.codec.types.Observation();
	taskSpec='';
end    

function theObservation=test_message_environment_start()
    global test_message_environment_struct;

	theObservation=test_message_environment_struct.emptyObservation;
end


function rewardObservation=test_message_environment_step(theAction)
    global test_message_environment_struct;

	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal();
	rewardObservation.o=test_message_environment_struct.emptyObservation;
end

function returnMessage=test_message_environment_message(theMessageJavaObject)
    theMessage=char(theMessageJavaObject);

	if isempty(theMessage)
		returnMessage='empty';
		return
	end

	if strcmp(theMessage,'null')
		returnMessage='';
		return
	end

	if strcmp(theMessage,'empty')
		returnMessage='';
		return
	end
	
	returnMessage=sprintf('%s',theMessage);
end