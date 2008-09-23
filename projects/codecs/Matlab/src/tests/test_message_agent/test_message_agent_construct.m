function theAgent=test_message_agent_construct()
    theAgent.agent_init=@test_message_agent_init;
    theAgent.agent_start=@test_message_agent_start;
    theAgent.agent_step=@test_message_agent_step;
    theAgent.agent_end=@test_message_agent_end;
    theAgent.agent_cleanup=@test_message_agent_cleanup;
    theAgent.agent_message=@test_message_agent_message;
end