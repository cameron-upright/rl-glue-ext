function theAgent=test_message_agent()
    theAgent.agent_init=@test_message_agent_init;
    theAgent.agent_start=@test_message_agent_start;
    theAgent.agent_step=@test_message_agent_step;
    theAgent.agent_end=@test_message_agent_end;
    theAgent.agent_cleanup=@test_message_agent_cleanup;
    theAgent.agent_message=@test_message_agent_message;
end

function test_message_agent_init(taskSpec)
end    

function theAction=test_message_agent_start(theObservation)
	theAction=org.rlcommunity.rlglue.codec.types.Action(0,0,0);
end

function theAction=test_message_agent_step(theReward, theObservation)
	theAction=org.rlcommunity.rlglue.codec.types.Action(0,0,0);
end

function test_message_agent_end(theReward)
end

function test_message_agent_cleanup()
end

function returnMessage=test_message_agent_message(theMessageJavaObject)
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
