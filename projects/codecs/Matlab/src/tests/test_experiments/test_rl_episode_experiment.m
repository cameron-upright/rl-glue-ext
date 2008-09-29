function failures=test_rl_episode_experiment()
        failures=0;
        totalTests=0;
        
        RL_init();
        
        terminal=RL_episode(0);
    
         	[failures,totalTests]=check_fail(terminal~=1,failures,totalTests);
         	[failures,totalTests]=check_fail(RL_num_steps()~=5,failures,totalTests);

        terminal=RL_episode(1);
        
         	[failures,totalTests]=check_fail(terminal~=0,failures,totalTests);
         	[failures,totalTests]=check_fail(RL_num_steps()~=1,failures,totalTests);

            
        terminal=RL_episode(2);
        
         	[failures,totalTests]=check_fail(terminal~=0,failures,totalTests);
         	[failures,totalTests]=check_fail(RL_num_steps()~=2,failures,totalTests);

        terminal=RL_episode(4);
        
         	[failures,totalTests]=check_fail(terminal~=0,failures,totalTests);
         	[failures,totalTests]=check_fail(RL_num_steps()~=4,failures,totalTests);

        terminal=RL_episode(5);
        
         	[failures,totalTests]=check_fail(terminal~=0,failures,totalTests);
         	[failures,totalTests]=check_fail(RL_num_steps()~=5,failures,totalTests);

            terminal=RL_episode(6);
        
         	[failures,totalTests]=check_fail(terminal~=1,failures,totalTests);
         	[failures,totalTests]=check_fail(RL_num_steps()~=5,failures,totalTests);
            
            terminal=RL_episode(7);
        
         	[failures,totalTests]=check_fail(terminal~=1,failures,totalTests);
         	[failures,totalTests]=check_fail(RL_num_steps()~=5,failures,totalTests);



            
        if(failures>0)
            fprintf(1,'Failed %d of %d tests in test_rl_episode_experiment\n',failures,totalTests);
        else
            fprintf(1,'Passed ALL  %d tests in test_rl_episode_experiment\n',totalTests);
        end
        
        disconnectGlue();
end


function [failures, totalTests]=check_fail(theCondition, failures, totalTests)
    totalTests=totalTests+1;
    if(theCondition)
        fprintf(1,'Failed test %d\n',totalTests);
        failures=failures+1;
    end
end