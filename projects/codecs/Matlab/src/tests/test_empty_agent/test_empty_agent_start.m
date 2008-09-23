function theAction=test_empty_agent_start(theObservation)
    global test_empty_agent_struct;
    test_empty_agent_struct.whichEpisode=test_empty_agent_struct.whichEpisode+1;

	if mod(test_empty_agent_struct.whichEpisode,2)==0
		theAction=test_empty_agent_struct.emptyAction;
	else
		theAction=test_empty_agent_struct.nonEmptyAction;
end