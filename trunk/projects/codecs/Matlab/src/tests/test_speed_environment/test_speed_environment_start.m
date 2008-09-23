function theObservation=test_speed_environment_start()
    global test_speed_environment_struct;

	test_speed_environment_struct.whichEpisode=test_speed_environment_struct.whichEpisode+1;
	test_speed_environment_struct.stepCount=0;
	test_speed_environment_struct.o=org.rlcommunity.rlglue.codec.types.Observation;
	theObservation=test_speed_environment_struct.o;
end
