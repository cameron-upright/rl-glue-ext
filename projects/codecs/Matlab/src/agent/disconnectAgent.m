function disconnectAgent(network)
    global p__rlglueAgentStruct;
    
    if isfield(p__rlglueAgentStruct,'network')
        p__rlglueAgentStruct.network.close();
		p__rlglueAgentStruct=rmfield(p__rlglueAgentStruct,'network');
    end
    
end