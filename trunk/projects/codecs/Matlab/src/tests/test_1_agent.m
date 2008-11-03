function theAgent=test_1_agent()
    theAgent.agent_init=@test_1_agent_init;
    theAgent.agent_start=@test_1_agent_start;
    theAgent.agent_step=@test_1_agent_step;
    theAgent.agent_end=@test_1_agent_end;
    theAgent.agent_cleanup=@test_1_agent_cleanup;
    theAgent.agent_message=@test_1_agent_message;
end


function test_1_agent_init(taskSpec)
    global test_1_agent_struct;
    test_1_agent_struct.stepCount=0;
end    

function theAction=test_1_agent_start(theObservation)
    global test_1_agent_struct;
    test_1_agent_struct.stepCount=0;
    theAction = org.rlcommunity.rlglue.codec.types.Action(theObservation);
end


function theAction=test_1_agent_step(theReward, theObservation)
    global test_1_agent_struct;
    test_1_agent_struct.stepCount=test_1_agent_struct.stepCount+1;
    theAction = org.rlcommunity.rlglue.codec.types.Action(theObservation);
end

function test_1_agent_end(theReward)
end

function returnMessage=test_1_agent_message(theMessageJavaObject)
  
    global test_1_agent_struct;
    if(isempty(test_1_agent_struct))
        test_1_agent_struct.stepCount=0;
    end
    if(isempty(test_1_agent_struct.stepCount))
        test_1_agent_struct.stepCount=0;
    end
    
    theMessage=char(theMessageJavaObject);
    
    timesToPrint=mod(test_1_agent_struct.stepCount,3);
    
    returnMessage=sprintf('%s|',theMessage);
    %Start at one because the top end will be inclusive unlike C for loop
    for i=1:1:timesToPrint
        returnMessage=sprintf('%s%d.',returnMessage,test_1_agent_struct.stepCount);
    end
    returnMessage=sprintf('%s|%s',returnMessage,theMessage);
end

function test_1_agent_cleanup()
    global test_1_agent_struct;
end