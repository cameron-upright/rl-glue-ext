function rlglue_construct(Agent, Environment)
global rlglue__struct;

rlglue__struct.Agent=Agent;
rlglue__struct.Environment=Environment;

rlglue__struct.agent_init=str2func(Agent.agent_init);
rlglue__struct.agent_step=str2func(Agent.agent_step);
rlglue__struct.agent_start=str2func(Agent.agent_start);
rlglue__struct.agent_end=str2func(Agent.agent_end);
rlglue__struct.agent_cleanup=str2func(Agent.agent_cleanup);
rlglue__struct.agent_message=str2func(Agent.agent_message);
rlglue__struct.agent_freeze=str2func(Agent.agent_freeze);

rlglue__struct.env_init=str2func(Environment.env_init);
rlglue__struct.env_start=str2func(Environment.env_start);
rlglue__struct.env_step=str2func(Environment.env_step);
rlglue__struct.env_message=str2func(Environment.env_message);
rlglue__struct.env_cleanup=str2func(Environment.env_cleanup);
rlglue__struct.env_set_state=str2func(Environment.env_set_state);
rlglue__struct.env_get_state=str2func(Environment.env_get_state);
rlglue__struct.env_set_random_seed=str2func(Environment.env_set_random_seed);
rlglue__struct.env_get_random_seed=str2func(Environment.env_get_random_seed);


rlglue__struct.steps=0;
rlglue__struct.isTerminal=false;
rlglue__struct.totalReward=0;
rlglue__struct.totalEpisodes=0;
end