function connectEnviroment(theEnviroment)
    global p__rlglueEnvStruct;
    
    if (exist('p__rlglueEnvStruct'))
        if exist('p__rlglueEnvStruct.network')
            disconnectEnvironment();
        end
    end
    
    p__rlglueEnvStruct.theEnviroment=theEnviroment;
    host='localhost';
    port=4096;
    timeout=60;
    
    
    fprintf(1,'Connecting to rl_glue at host: %s on port %d\n', host, port);

    p__rlglueEnvStruct.network=org.rlcommunity.rlglue.codec.network.Network;
    p__rlglueEnvStruct.network.connect(host,port,timeout);

    p__rlglueEnvStruct.network.clearSendBuffer();
    p__rlglueEnvStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kEnvironmentConnection);
    p__rlglueEnvStruct.network.putInt(0);% No body to this packet
    p__rlglueEnvStruct.network.flipSendBuffer();
    p__rlglueEnvStruct.network.send();
end