function RL_load_random_seed(sk)
    global p__rlglueStruct;

    p__rlglueStruct.network.clearSendBuffer();
    p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kRLSetRandomSeed);
    p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(sk));
    p__rlglueStruct.network.putRandomSeedKey(sk);
    p__rlglueStruct.network.flipSendBuffer();
    p__rlglueStruct.network.send();

    doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLSetRandomSeed);
end
