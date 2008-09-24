function disconnectEnvironment(network)
    global p__rlglueEnvStruct;
    
	if isfield(p__rlglueEnvStruct,'network')
	    p__rlglueEnvStruct.network.close();
		p__rlglueEnvStruct=rmfield(p__rlglueEnvStruct,'network');
	end

end