function failures=test_sanity_experiment()
        failures=0;
        totalTests=0;
        
        task_spec=char(RL_init());
        
        [failures, totalTests]=check_fail(~strcmp(task_spec,'sample task spec'),failures,totalTests);

            
        if(failures>0)
            fprintf(1,'Failed %d of %d tests in test_sanity_experiment\n',failures,totalTests);
        else
            fprintf(1,'Passed ALL  %d tests in test_sanity_experiment\n',totalTests);
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