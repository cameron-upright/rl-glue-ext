function failures=test_message_experiment()
        failures=0;
        totalTests=0;
        
        RL_init();
        

       [failures,totalTests]=check_fail(~strcmp('empty',RL_env_message('')),failures,totalTests);
       [failures,totalTests]=check_fail(~strcmp('empty',RL_agent_message('')),failures,totalTests);

       [failures,totalTests]=check_fail(~strcmp('',RL_env_message('empty')),failures,totalTests);
       [failures,totalTests]=check_fail(~strcmp('',RL_agent_message('empty')),failures,totalTests);

       [failures,totalTests]=check_fail(~strcmp('',RL_env_message('null')),failures,totalTests);
       [failures,totalTests]=check_fail(~strcmp('',RL_agent_message('null')),failures,totalTests);

       
       [failures,totalTests]=check_fail(~strcmp('1',RL_env_message('1')),failures,totalTests);
       [failures,totalTests]=check_fail(~strcmp('1',RL_agent_message('1')),failures,totalTests);

       [failures,totalTests]=check_fail(~strcmp('1000000000000000000000',RL_env_message('1000000000000000000000')),failures,totalTests);
       [failures,totalTests]=check_fail(~strcmp('1000000000000000000000',RL_agent_message('1000000000000000000000')),failures,totalTests);

       [failures,totalTests]=check_fail(~strcmp('21111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111113',RL_env_message('21111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111113')),failures,totalTests);
       [failures,totalTests]=check_fail(~strcmp('45555555555555555555555555555555555555555555555555555555655555555555555555555555555555555555555555555555555555559',RL_agent_message('45555555555555555555555555555555555555555555555555555555655555555555555555555555555555555555555555555555555555559')),failures,totalTests);

       [failures,totalTests]=check_fail(~strcmp('2',RL_env_message('2')),failures,totalTests);
       [failures,totalTests]=check_fail(~strcmp('2',RL_agent_message('2')),failures,totalTests);


	
        
        
        
        
        
        

            
        if(failures>0)
            fprintf(1,'Failed %d of %d tests in test_message_experiment\n',failures,totalTests);
        else
            fprintf(1,'Passed ALL  %d tests in test_message_experiment\n',totalTests);
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