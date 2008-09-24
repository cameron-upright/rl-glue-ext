function response=RL_env_message(message)
		forceConnection();
        global p__rlglueStruct;

        p__rlglueStruct.network.clearSendBuffer();
        p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kRLEnvMessage);
        p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.sizeOf(message));
        p__rlglueStruct.network.putString(message);
        p__rlglueStruct.network.flipSendBuffer();
        p__rlglueStruct.network.send();

        doStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLEnvMessage);
        response = p__rlglueStruct.network.getString();
end