function theAction=test_empty_agent_step(theReward, theObservation)
    global test_empty_agent_struct;

	if mod(test_empty_agent_struct.whichEpisode,2)==0
		theAction=test_empty_agent_struct.emptyAction;
	else
		theAction=test_empty_agent_struct.nonEmptyAction;
end