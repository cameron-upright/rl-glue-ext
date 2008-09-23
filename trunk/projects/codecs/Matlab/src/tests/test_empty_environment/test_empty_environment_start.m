function theObservation=test_empty_environment_start()
    global test_empty_environment_struct;

	test_empty_environment_struct.whichEpisode=test_empty_environment_struct.whichEpisode+1;

	if mod(test_empty_environment_struct.whichEpisode,2) == 0
		theObservation=test_empty_environment_struct.emptyObservation;
	else
		theObservation=test_empty_environment_struct.nonEmptyObservation;
	end
end
