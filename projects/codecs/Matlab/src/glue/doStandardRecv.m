function doStandardRecv(state)
    global p__rlglueStruct;
		p__rlglueStruct.network.clearRecvBuffer();
		
		recvSize = p__rlglueStruct.network.recv(8) - 8;

		glueState = p__rlglueStruct.network.getInt(0);
		dataSize = p__rlglueStruct.network.getInt(org.rlcommunity.rlglue.codec.network.Network.kIntSize);
		remaining = dataSize - recvSize;

		if remaining < 0
			remaining = 0;
        end
		
		remainingReceived=p__rlglueStruct.network.recv(remaining);

		p__rlglueStruct.network.flipRecvBuffer();	
		
		% Discard the header - we should have a more elegant method for doing this.
		p__rlglueStruct.network.getInt();
		p__rlglueStruct.network.getInt();
		
		if glueState ~= state
			fprintf(2,'Not synched with server. glueState = %d but should be %d\n',glueState,state);
            exit(1);
        end
    end
        
       
