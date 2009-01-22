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
function connectAgent(theAgent)
    checkForJavaCodec();

    global p__rlglueAgentStruct;
    if isfield(p__rlglueAgentStruct,'network')
		disconnectAgent();
    end

    p__rlglueAgentStruct.theAgent=theAgent;
    host='localhost';
    port=4096;
    timeout=60;
    
    
    fprintf(1,'RL-Glue Matlab Agent Codec Version: %s (%s)\n',RL_get_codec_version(),RL_get_svn_version());
    fprintf(1,'\tConnecting to rl_glue at host: %s on port %d\n', host, port);

    p__rlglueAgentStruct.network=org.rlcommunity.rlglue.codec.network.Network;
    p__rlglueAgentStruct.network.connect(host,port,timeout);
    
    fprintf(1,'\tAgent Codec Connected\n');

    p__rlglueAgentStruct.network.clearSendBuffer();
    p__rlglueAgentStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kAgentConnection);
    p__rlglueAgentStruct.network.putInt(0);% No body to this packet
    p__rlglueAgentStruct.network.flipSendBuffer();
    p__rlglueAgentStruct.network.send();
end