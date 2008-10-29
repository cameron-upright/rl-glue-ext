function theAgent=test_empty_agent()
    theAgent.agent_init=@test_empty_agent_init;
    theAgent.agent_start=@test_empty_agent_start;
    theAgent.agent_step=@test_empty_agent_step;
    theAgent.agent_end=@test_empty_agent_end;
    theAgent.agent_cleanup=@test_empty_agent_cleanup;
    theAgent.agent_message=@test_empty_agent_message;
end

function test_empty_agent_init(taskSpec)
    global test_empty_agent_struct;

	test_empty_agent_struct.whichEpisode=0;
	test_empty_agent_struct.emptyAction=org.rlcommunity.rlglue.codec.types.Action(0,0,0);
	test_empty_agent_struct.nonEmptyAction=org.rlcommunity.rlglue.codec.types.Action(7,3,1);

	test_empty_agent_struct.nonEmptyAction.intArray=[0 1 2 3 4 5 6];
	test_empty_agent_struct.nonEmptyAction.doubleArray=[0/3 1/3 2/3];
	test_empty_agent_struct.nonEmptyAction.charArray=['a'];
end    


function theAction=test_empty_agent_start(theObservation)
    global test_empty_agent_struct;
    test_empty_agent_struct.whichEpisode=test_empty_agent_struct.whichEpisode+1;

	if mod(test_empty_agent_struct.whichEpisode,2)==0
		theAction=test_empty_agent_struct.emptyAction;
	else
		theAction=test_empty_agent_struct.nonEmptyAction;
	end
end

function theAction=test_empty_agent_step(theReward, theObservation)
    global test_empty_agent_struct;

	if mod(test_empty_agent_struct.whichEpisode,2)==0
		theAction=test_empty_agent_struct.emptyAction;
	else
		theAction=test_empty_agent_struct.nonEmptyAction;
	end
end

function test_empty_agent_end(theReward)
end

function test_empty_agent_cleanup()
    global test_empty_agent_struct;
end

function returnMessage=test_empty_agent_message(theMessageJavaObject)
	returnMessage='';
end