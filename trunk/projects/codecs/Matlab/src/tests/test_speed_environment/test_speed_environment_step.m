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
	