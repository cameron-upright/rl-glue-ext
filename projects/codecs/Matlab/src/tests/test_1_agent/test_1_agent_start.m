function theAction=test_1_agent_start(theObservation)
    global test_1_agent_struct;
    test_1_agent_struct.stepCount=0;
    theAction = org.rlcommunity.rlglue.codec.types.Action(theObservation);
end