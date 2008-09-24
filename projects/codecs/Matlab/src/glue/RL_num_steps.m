function steps=RL_num_steps()
        global p__rlglueStruct;

        doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLNumSteps);
        doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLNumSteps);
        steps = p__rlglueStruct.network.getInt();
end