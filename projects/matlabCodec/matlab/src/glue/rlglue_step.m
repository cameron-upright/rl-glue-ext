function rlglue_step()
    global rlglue__struct;
    
    [Obs, Reward] = rlglue__struct.Env.env_step(rlglue__struct.lastAction);
    
    lastAction=rlglue__struct.Agent.agent_step(Obs,Reward);
end