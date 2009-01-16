function skeleton_experiment()
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

