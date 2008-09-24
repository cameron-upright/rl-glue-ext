function taskSpec=RL_init()
		forceConnection();
        global p__rlglueStruct;
	
        doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLInit);
		doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLInit);
        taskSpec=p__rlglueStruct.network.getString();
end