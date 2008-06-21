function RL_episode(maxStepsThisEpisode)
    global rlglue__struct;
    
    RL_start();

    currentStep=1;
    while(~rlglue__struct.isTerminal && (maxStepsThisEpisode<=0 || currentStep<maxStepsThisEpisode))
        RL_step();
        currentStep=currentStep+1;
    end
    
end