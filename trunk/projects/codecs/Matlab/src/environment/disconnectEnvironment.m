function disconnectEnvironment(network)
    global p__rlglueEnvStruct;
    
    if(~isempty(p__rlglueEnvStruct.network))
        p__rlglueEnvStruct.network.close();
        clear p__rlglueEnvStruct.network;
    end
    
end