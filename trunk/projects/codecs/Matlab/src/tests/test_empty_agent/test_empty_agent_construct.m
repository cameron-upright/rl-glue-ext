function theAgent=test_empty_agent_construct()
    theAgent.agent_init=@test_empty_agent_init;
    theAgent.agent_start=@test_empty_agent_start;
    theAgent.agent_step=@test_empty_agent_step;
    theAgent.agent_end=@test_empty_agent_end;
    theAgent.agent_cleanup=@test_empty_agent_cleanup;
    theAgent.agent_message=@test_empty_agent_message;
end