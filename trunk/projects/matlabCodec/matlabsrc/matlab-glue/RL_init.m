function RL_init()
global rlglue__struct;
    taskSpec=rlglue__struct.env_init();
    rlglue__struct.agent_init(taskSpec);
    rlglue__struct.totalEpisodes=0;
end