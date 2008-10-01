function theAgent=skeleton_agent_construct()
    theAgent.agent_init=@skeleton_agent_init;
    theAgent.agent_start=@skeleton_agent_start;
    theAgent.agent_step=@skeleton_agent_step;
    theAgent.agent_end=@skeleton_agent_end;
    theAgent.agent_cleanup=@skeleton_agent_cleanup;
    theAgent.agent_message=@skeleton_agent_message;
end