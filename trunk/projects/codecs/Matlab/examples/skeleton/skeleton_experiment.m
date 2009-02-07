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
function skeleton_experiment()
	fprintf(1,'Starting Sample Experiment.  Remember that currently the Matlab codec can only run\n');
	fprintf(1,'ONE agent or ONE environment or ONE experiment per Matlab instance.  To connect this experiment\n');
	fprintf(1,'to anything you will need to run them in a separate Matlab instance or using a different codec.\n');
	fprintf(1,'If running them all in one Matlab instance is very important to you, please go to the \n');
	fprintf(1,'following URL and click the star to show your interest:\n');
	fprintf(1,'http://code.google.com/p/rl-glue-ext/issues/detail?id=57\n\n');

    	taskSpec = RL_init();
        global whichEpisode;
        whichEpisode = 0;

        fprintf(1,'Experiment starting up!\n');
        taskSpec = RL_init();
        fprintf(1,'RL_init called, the environment sent task spec: %s\n',char(taskSpec));

        fprintf(1,'\n\n----------Sending some sample messages----------\n');

        %Talk to the agent and environment a bit...
        responseMessage = RL_agent_message('what is your name?');
        fprintf(1,'Agent responded to \''what is your name?\'' with: %s\n',char(responseMessage));

        responseMessage = RL_agent_message('If at first you don\''t succeed; call it version 1.0');
        fprintf(1,'Agent responded to \''If at first you do\''t succeed; call it version 1.0  \'' with: %s\n',char(responseMessage));

        responseMessage = RL_env_message('what is your name?');
        fprintf(1,'Environment responded to \''what is your name?\'' with: %s\n',char(responseMessage));
        responseMessage = RL_env_message('If at first you don\''t succeed; call it version 1.0');
        fprintf(1,'Environment responded to \''If at first you don\''t succeed; call it version 1.0  \'' with: %s\n',char(responseMessage));

        fprintf(1,'\n\n----------Running a few episodes----------\n');
        runEpisode(100);
        runEpisode(100);
        runEpisode(100);
        runEpisode(100);
        runEpisode(100);
        runEpisode(1);
        %  Remember that stepLimit of 0 means there is no limit at all!
        runEpisode(0);
        RL_cleanup();

        fprintf(1,'\n\n----------Stepping through an episode----------\n');
        % We could also start over and do another experiment 
        taskSpec = RL_init();

         % We could run one step at a time instead of one episode at a time 
         % Start the episode 
        startResponse = RL_start();

        firstObservation = startResponse.o.intArray(1);
        firstAction = startResponse.a.intArray(1);
        fprintf(1,'First observation and action were: %d and: %d\n',firstObservation,firstAction);

        % Run one step 
        stepResponse = RL_step();

        % Run until the episode ends
        while (stepResponse.terminal ~= 1) 
            stepResponse = RL_step();
            if (stepResponse.terminal ~= 1) 
                % Could optionally print state,action pairs 
                % printf('(%d,%d) ',stepResponse.o.intArray[0],stepResponse.a.intArray[0]);
            end
        end

        fprintf(1,'\n\n----------Summary----------\n');

        totalSteps = RL_num_steps();
        totalReward = RL_return();
        fprintf(1,'It ran for %d, total reward was: %f\n',totalSteps,totalReward);
        RL_cleanup();
        
        disconnectGlue();
    end

    %  Run One Episode of length maximum cutOff
function runEpisode(stepLimit) 
	global whichEpisode;
    terminal = RL_episode(stepLimit);

    totalSteps = RL_num_steps();
    totalReward = RL_return();

    fprintf(1,'Episode %d\t %d steps \t %f total reward\t natural end %d\n',whichEpisode,totalSteps,totalReward,terminal);

    whichEpisode=whichEpisode+1;
end

