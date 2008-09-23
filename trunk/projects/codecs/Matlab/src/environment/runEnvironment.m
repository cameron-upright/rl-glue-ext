function runEnvironment(theEnvironment)
    connectEnvironment(theEnvironment)
    shouldStop=false;
    while(~shouldStop)
        shouldStop=runEnvironmentLoop();
    end
end