function exitStatus=RL_episode(numSteps)
    global p__rlglueStruct;

    p__rlglueStruct.network.clearSendBuffer();
    p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kRLEpisode);
    p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kIntSize);
    p__rlglueStruct.network.putInt(numSteps);
    p__rlglueStruct.network.flipSendBuffer();
    p__rlglueStruct.network.send();

    doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLEpisode);
                exitStatus=p__rlglueStruct.network.getInt();
end