function doCallWithNoParams(state)
    global p__rlglueStruct;

		p__rlglueStruct.network.clearSendBuffer();
		p__rlglueStruct.network.putInt(state);
		p__rlglueStruct.network.putInt(0);
		p__rlglueStruct.network.flipSendBuffer();
		p__rlglueStruct.network.send();
end
