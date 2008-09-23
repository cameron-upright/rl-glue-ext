function test_empty_agent_init(taskSpec)
    global test_empty_agent_struct;

	test_empty_agent_struct.whichEpisode=0;
	test_empty_agent_struct.emptyAction=org.rlcommunity.rlglue.codec.types.Action(0,0,0);
	test_empty_agent_struct.nonEmptyAction=org.rlcommunity.rlglue.codec.types.Action(7,3,1);

	test_empty_agent_struct.nonEmptyAction.intArray=[0 1 2 3 4 5 6];
	test_empty_agent_struct.nonEmptyAction.doubleArray=[0/3 1/3 2/3];
	test_empty_agent_struct.nonEmptyAction.charArray=['a'];
end    
   