function obsact=RL_start()
    global p__rlglueStruct;
			doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLStart);
			doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLStart);

            obsact = org.rlcommunity.rlglue.codec.types.Observation_action;

			obsact.o = p__rlglueStruct.network.getObservation();
			obsact.a = p__rlglueStruct.network.getAction();
end
