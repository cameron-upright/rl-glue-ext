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
function connectEnviroment(theEnviroment)
	%NOTE: If the java package isn't in your path already, this WILL delete all your globals
	%because it calls javaddpath which calls clear('java')
    checkForJavaCodec();
    global p__rlglueEnvStruct;
	global p__rlglueStruct;
    
    if isfield(p__rlglueEnvStruct,'network')
		disconnectEnvironment();
    end
    
    p__rlglueEnvStruct.theEnviroment=theEnviroment;

%Set defaults for host and port
    host=char(org.rlcommunity.rlglue.codec.network.Network.kDefaultHost);
    port=org.rlcommunity.rlglue.codec.network.Network.kDefaultPort;
    timeout=org.rlcommunity.rlglue.codec.network.Network.kRetryTimeout;

%Pick up user specifications if there are any
	if isfield(p__rlglueStruct,'port')
		port=p__rlglueStruct.port;
	end
	if isfield(p__rlglueStruct,'host')
		host=p__rlglueStruct.host;
	end
    
    
    fprintf(1,'RL-Glue Matlab Environment Codec Version: %s (%s)\n',RL_get_codec_version(),RL_get_svn_version());
    fprintf(1,'\tConnecting to rl_glue at host: %s on port %d\n', host, port);

    p__rlglueEnvStruct.network=org.rlcommunity.rlglue.codec.network.Network;
    p__rlglueEnvStruct.network.connect(host,port,timeout);

    fprintf(1,'\tEnvironment Codec Connected\n');

    p__rlglueEnvStruct.network.clearSendBuffer();
    p__rlglueEnvStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kEnvironmentConnection);
    p__rlglueEnvStruct.network.putInt(0);% No body to this packet
    p__rlglueEnvStruct.network.flipSendBuffer();
    p__rlglueEnvStruct.network.send();
end