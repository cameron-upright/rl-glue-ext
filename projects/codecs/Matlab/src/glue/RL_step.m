function roat=RL_step()

    global p__rlglueStruct;

    doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLStep);
    doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLStep);

    roat = org.rlcommunity.rlglue.codec.types.Reward_observation_action_terminal;
    terminal=p__rlglueStruct.network.getInt();
    reward=p__rlglueStruct.network.getDouble();
    o=p__rlglueStruct.network.getObservation();
    a=p__rlglueStruct.network.getAction();
    roat.terminal = terminal;
    roat.r = reward;
    roat.o = o;
    roat.a = a;
    roat.o
end
