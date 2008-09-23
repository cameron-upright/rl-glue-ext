function theAction=test_1_agent_step(theReward, theObservation)
    global test_1_agent_struct;
    test_1_agent_struct.stepCount=test_1_agent_struct.stepCount+1;
    theAction = org.rlcommunity.rlglue.codec.types.Action(theObservation);
end