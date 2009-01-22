%  Copyright 2008 Brian Tanner
%  http://rl-glue-ext.googlecode.com/
%  brian@tannerpages.com
%  http://brian.tannerpages.com
%  
%   Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%  
%       http://www.apache.org/licenses/LICENSE-2.0
%  
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%  
%   $Revision$
%   $Date$
%   $Author$
%  $HeadURL$
%
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
        
       
