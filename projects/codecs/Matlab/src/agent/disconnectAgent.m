function disconnectAgent(network)
    global p__rlglueAgentStruct;
    
    if(~isempty(p__rlglueAgentStruct.network))
        p__rlglueAgentStruct.network.close();
        clear p__rlglueAgentStruct.network;
    end
    
end