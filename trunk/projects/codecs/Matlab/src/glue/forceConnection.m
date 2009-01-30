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
function forceConnection()
    global p__rlglueStruct;
           
    if ~isfield(p__rlglueStruct,'network')
	    checkForJavaCodec();
        p__rlglueStruct.network = org.rlcommunity.rlglue.codec.network.Network;


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

        fprintf(1,'RL-Glue Matlab Experiment Codec Version: %s (%s)\n',RL_get_codec_version(),RL_get_svn_version());
        fprintf(1,'\tConnecting to rl_glue at host: %s on port %d\n', char(host), port);

        % Connect
        p__rlglueStruct.network.connect(host,port,timeout);

        fprintf(1,'\tExperiment Codec Connected\n');
        p__rlglueStruct.network.clearSendBuffer();
        p__rlglueStruct.network.putInt(org.rlcommunity.rlglue.codec.network.Network.kExperimentConnection);
        p__rlglueStruct.network.putInt(0);
        p__rlglueStruct.network.flipSendBuffer();

        p__rlglueStruct.network.send();
    end
end
