function runAgent(theAgent)
    connectAgent(theAgent)
    shouldStop=false;
    while(~shouldStop)
        shouldStop=runAgentLoop();
    end
end