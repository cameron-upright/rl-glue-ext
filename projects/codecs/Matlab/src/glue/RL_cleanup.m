%  Copyright 2008 Brian Tanner
%  http://rl-glue-ext.googlecode.com/
%  brian@tannerpages.com
%  http://research.tannerpages.com
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
function RL_cleanup()
	doCallWithNoParams(org.rlcommunity.rlglue.codec.network.Network.kRLCleanup);
    
    %If there is an environment (if we're running more than one
    %component together), then make sure it executes (env_cleanup)
    ensureEnvExecutesIfNecessary();

    %If there is an agent (if we're running more than one
    %component together), then make sure it executes (agent_cleanup)
    ensureAgentExecutesIfNecessary();
  
    forceStandardRecv(org.rlcommunity.rlglue.codec.network.Network.kRLCleanup);
end