function failures=test_empty_experiment()
        failures=0;
        totalTests=0;
        
        RL_init();
        

        for whichEpisode = [1:1:4]
            startTuple=RL_start();
            
            if mod(whichEpisode,2)==0
                [failures,totalTests]=check_fail(length(startTuple.a.intArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.a.doubleArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.a.charArray)~=0,failures,totalTests);

                [failures,totalTests]=check_fail(length(startTuple.o.intArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.o.doubleArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.o.charArray)~=0,failures,totalTests);
            else
                [failures,totalTests]=check_fail(length(startTuple.a.intArray)~=7,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.a.doubleArray)~=3,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.a.charArray)~=1,failures,totalTests);

                [failures,totalTests]=check_fail(length(startTuple.o.intArray)~=2,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.o.doubleArray)~=4,failures,totalTests);
                [failures,totalTests]=check_fail(length(startTuple.o.charArray)~=5,failures,totalTests);
            end
        for whichStep=0:1:4
    		stepTuple=RL_step();
            [failures,totalTests]=check_fail(stepTuple.terminal~=0,failures,totalTests);
            [failures,totalTests]=check_fail(stepTuple.terminal~=0,failures,totalTests);

            if mod(whichEpisode,2)==0
                [failures,totalTests]=check_fail(length(stepTuple.a.intArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.a.doubleArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.a.charArray)~=0,failures,totalTests);

                [failures,totalTests]=check_fail(length(stepTuple.o.intArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.o.doubleArray)~=0,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.o.charArray)~=0,failures,totalTests);
            else
                [failures,totalTests]=check_fail(length(stepTuple.a.intArray)~=7,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.a.doubleArray)~=3,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.a.charArray)~=1,failures,totalTests);

                [failures,totalTests]=check_fail(length(stepTuple.o.intArray)~=2,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.o.doubleArray)~=4,failures,totalTests);
                [failures,totalTests]=check_fail(length(stepTuple.o.charArray)~=5,failures,totalTests);
            end
        end

	
	

        end

        
        
        
        
        
        

            
        if(failures>0)
            fprintf(1,'Failed %d of %d tests in test_empty_experiment\n',failures,totalTests);
        else
            fprintf(1,'Passed ALL  %d tests in test_empty_experiment\n',totalTests);
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