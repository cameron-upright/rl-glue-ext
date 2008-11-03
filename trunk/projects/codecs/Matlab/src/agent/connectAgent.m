function connectAgent(theAgent)
    global p__rlglueAgentStruct;
    
    if isfield(p__rlglueAgentStruct,'network')
		disconnectAgent();
    end
    
    p__rlglueAgentStruct.theAgent=theAgent;
    host='localhost';
    port=4096;
    timeout=60;
    
    fprintf(1,'RL-Glue Matlab Agent Codec Version: %s (%s)\n',RL_get_codec_version(),RL_get_svn_version());
    fprintf(1,'\tConnecting to rl_glue at host: %s on port %d\n', host, port);

    p__rlglueAgentStruct.network=org.rlcommunity.rlglue.codec.network.Network;
    p__rlglueAgentStruct.network.connect(host,port,timeout);
    
    fprintf(1,'\tAgent Codec Connected\n');

    p__rlglueAgentStruct.network.clearSendBuffer();
    p__rlglueAgentStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kAgentConnection);
    p__rlglueAgentStruct.network.putInt(0);% No body to this packet
    p__rlglueAgentStruct.network.flipSendBuffer();
    p__rlglueAgentStruct.network.send();
end