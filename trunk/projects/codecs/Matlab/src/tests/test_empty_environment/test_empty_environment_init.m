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
   