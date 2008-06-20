function rlglue_construct(Agent, Environment)
global rlglue__struct;

rlglue__struct.Agent=Agent;
rlglue__struct.Environment=Environment;

rlglue__struct.steps=0;
rlglue__struct.isTerminal=false;
rlglue__struct.totalReward=0;
rlglue__struct.totalEpisodes=0;
end