function forceConnection()
    global p__rlglueStruct;
           
           if ~isfield(p__rlglueStruct,'network')
			p__rlglueStruct.network = org.rlcommunity.rlglue.codec.network.Network;

			% Connect
			p__rlglueStruct.network.connect(org.rlcommunity.rlglue.codec.network.Network.kDefaultHost,org.rlcommunity.rlglue.codec.network.Network.kDefaultPort,org.rlcommunity.rlglue.codec.network.Network.kRetryTimeout);

			p__rlglueStruct.network.clearSendBuffer();
			p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kExperimentConnection);
			p__rlglueStruct.network.putInt(0);
			p__rlglueStruct.network.flipSendBuffer();

			p__rlglueStruct.network.send();
        end
end
