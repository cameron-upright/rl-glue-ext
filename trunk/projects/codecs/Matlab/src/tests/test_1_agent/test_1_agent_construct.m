function theAgent=test_1_agent_construct()
    theAgent.agent_init=@test_1_agent_init;
    theAgent.agent_start=@test_1_agent_start;
    theAgent.agent_step=@test_1_agent_step;
    theAgent.agent_end=@test_1_agent_end;
    theAgent.agent_cleanup=@test_1_agent_cleanup;
    theAgent.agent_message=@test_1_agent_message;
end