function RL_cleanup()
	doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLCleanup);
    doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLCleanup);
end