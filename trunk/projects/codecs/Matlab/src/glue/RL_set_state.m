function RL_set_state(sk)
    global p__rlglueStruct;

    p__rlglueStruct.network.clearSendBuffer();
    p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kRLSetState);
    p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(sk));
    p__rlglueStruct.network.putStateKey(sk);
    p__rlglueStruct.network.flipSendBuffer();
    p__rlglueStruct.network.send();

    doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLSetState);
end
