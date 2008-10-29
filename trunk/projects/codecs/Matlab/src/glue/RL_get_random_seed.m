function randomSeedKey=RL_save_random_seed()
    global p__rlglueStruct;
    doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLGetRandomSeed);
    doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLGetRandomSeed);
    randomSeedKey=p__rlglueStruct.network.getRandomSeedKey();
end
