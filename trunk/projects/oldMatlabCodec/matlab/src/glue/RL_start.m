function Observation_action=RL_start(theMessage)
global rlglue__struct;

    rlglue__struct.steps=1;
    rlglue__struct.isTerminal=false;
    rlglue__struct.totalReward=0;
    
    theObservation=rlglue__struct.env_start();
    rlglue__struct.lastAction=rlglue__struct.agent_start(theObservation);
    
    Observation_action.Observation=theObservation;
    Observation_action.Action=rlglue__struct.lastAction;
end