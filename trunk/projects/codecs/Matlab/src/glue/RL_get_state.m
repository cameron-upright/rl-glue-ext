function stateKey=RL_save_state()
    global p__rlglueStruct;
    doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLGetState);
    doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLGetState);
    stateKey=p__rlglueStruct.network.getStateKey();
end
