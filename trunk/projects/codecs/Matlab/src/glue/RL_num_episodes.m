function episodes=RL_num_episodes()
        global p__rlglueStruct;

        doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLNumEpisodes);
        doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLNumEpisodes);
        episodes = p__rlglueStruct.network.getInt();
end