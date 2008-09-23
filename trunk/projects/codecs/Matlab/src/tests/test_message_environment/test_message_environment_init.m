function taskSpec=test_message_environment_init()
    global test_message_environment_struct;

	test_message_environment_struct.seed=org.rlcommunity.rlglue.codec.types.Random_seed_key();
	test_message_environment_struct.state=org.rlcommunity.rlglue.codec.types.State_key();
	

	test_message_environment_struct.emptyObservation=org.rlcommunity.rlglue.codec.types.Observation();
	taskSpec='';
end    
   