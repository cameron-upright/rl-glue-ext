function rewardObservation=test_1_environment_step(theAction)
    global test_1_environment_struct;

	theObservation = org.rlcommunity.rlglue.codec.types.Observation();
	theObservation.intArray=[test_1_environment_struct.stepCount];

    test_1_environment_struct.stepCount=test_1_environment_struct.stepCount+1;

	terminal=0;
	
	if test_1_environment_struct.stepCount==5
		terminal=1;
    end
    
	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation(1.0,theObservation,terminal);
	
end