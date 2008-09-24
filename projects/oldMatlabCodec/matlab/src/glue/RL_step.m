function ROAT=RL_step()
    global rlglue__struct;
    
    RO=rlglue__struct.env_step(rlglue__struct.lastAction);
    rlglue__struct.totalReward=rlglue__struct.totalReward+RO.reward;

    rlglue__struct.isTerminal=(RO.terminal==1);
    
    if(rlglue__struct.isTerminal)
        rlglue__struct.agent_end(RO.reward);
        rlglue__struct.totalEpisodes= rlglue__struct.totalEpisodes+1;
    else
        rlglue__struct.steps=rlglue__struct.steps+1;
        rlglue__struct.lastAction=rlglue__struct.agent_step(RO.reward,RO.Observation);
    end
    
    ROAT.reward=RO.reward;
    ROAT.Observation=RO.Observation;
    ROAT.Action=rlglue__struct.lastAction;
    ROAT.terminal=RO.terminal;

end