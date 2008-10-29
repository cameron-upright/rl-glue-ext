function connectEnviroment(theEnviroment)
    global p__rlglueEnvStruct;
    
    if isfield(p__rlglueEnvStruct,'network')
		disconnectEnvironment();
    end
    
    p__rlglueEnvStruct.theEnviroment=theEnviroment;
    host='localhost';
    port=4096;
    timeout=60;
    
    
    fprintf(1,'RL-Glue Matlab Environment Codec Version: %s (%s)\n',RL_save_codec_version(),RL_save_svn_version());
    fprintf(1,'\tConnecting to rl_glue at host: %s on port %d\n', host, port);

    p__rlglueEnvStruct.network=org.rlcommunity.rlglue.codec.network.Network;
    p__rlglueEnvStruct.network.connect(host,port,timeout);

    fprintf(1,'\tEnvironment Codec Connected\n');

    p__rlglueEnvStruct.network.clearSendBuffer();
    p__rlglueEnvStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kEnvironmentConnection);
    p__rlglueEnvStruct.network.putInt(0);% No body to this packet
    p__rlglueEnvStruct.network.flipSendBuffer();
    p__rlglueEnvStruct.network.send();
end