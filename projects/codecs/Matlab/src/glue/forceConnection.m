function forceConnection()
    global p__rlglueStruct;
           
    if ~isfield(p__rlglueStruct,'network')
        p__rlglueStruct.network = org.rlcommunity.rlglue.codec.network.Network;

        host=org.rlcommunity.rlglue.codec.network.Network.kDefaultHost;
        port=org.rlcommunity.rlglue.codec.network.Network.kDefaultPort;
        timeout=org.rlcommunity.rlglue.codec.network.Network.kRetryTimeout;

        fprintf(1,'RL-Glue Matlab Experiment Codec Version: %s (%s)\n',RL_get_codec_version(),RL_get_svn_version());
        fprintf(1,'\tConnecting to rl_glue at host: %s on port %d\n', char(host), port);

        % Connect
        p__rlglueStruct.network.connect(host,port,timeout);

        fprintf(1,'\tExperiment Codec Connected\n');
        p__rlglueStruct.network.clearSendBuffer();
        p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kExperimentConnection);
        p__rlglueStruct.network.putInt(0);
        p__rlglueStruct.network.flipSendBuffer();

        p__rlglueStruct.network.send();
    end
end
