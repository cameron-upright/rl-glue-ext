function reward=RL_return()
        global p__rlglueStruct;

        doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLReturn);
        doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLReturn);
        reward = p__rlglueStruct.network.getDouble();
end