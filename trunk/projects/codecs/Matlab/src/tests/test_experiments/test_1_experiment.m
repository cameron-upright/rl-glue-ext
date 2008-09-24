function failures=test_1_experiment()
        failures=0;
        totalTests=0;
        
        RL_init();
        

	RL_start();

    roat=RL_step();

	
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=1,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=0,failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('one|1.|one',RL_env_message('one')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('one|1.|one',RL_agent_message('one')),failures,totalTests);
 
 	[failures,totalTests]=check_fail(roat.terminal~=0,failures,totalTests);
	

	roat=RL_step();

    [failures,totalTests]=check_fail(~strcmp('two|2.2.|two',RL_env_message('two')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('two|2.2.|two',RL_agent_message('two')),failures,totalTests);
	[failures,totalTests]=check_fail(roat.terminal~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=1,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(1)~=1,failures,totalTests);

	roat=RL_step();

    [failures,totalTests]=check_fail(~strcmp('three||three',RL_env_message('three')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('three||three',RL_agent_message('three')),failures,totalTests);
	[failures,totalTests]=check_fail(roat.terminal~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=1,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(1)~=2,failures,totalTests);
    
	roat=RL_step();
    
    
    [failures,totalTests]=check_fail(~strcmp('four|4.|four',RL_env_message('four')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('four|4.|four',RL_agent_message('four')),failures,totalTests);
	[failures,totalTests]=check_fail(roat.terminal~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.intArray)~=1,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.doubleArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(length(roat.o.charArray)~=0,failures,totalTests);
	[failures,totalTests]=check_fail(roat.o.intArray(1)~=3,failures,totalTests);

	roat=RL_step();
    [failures,totalTests]=check_fail(~strcmp('five|5.5.|five',RL_env_message('five')),failures,totalTests);
    [failures,totalTests]=check_fail(~strcmp('five|4.|five',RL_agent_message('five')),failures,totalTests);
	[failures,totalTests]=check_fail(roat.terminal==0,failures,totalTests);
        
        
        
        
        
        
        
        
        
        

            
        if(failures>0)
            fprintf(1,'Failed %d of %d tests in test_1_experiment\n',failures,totalTests);
        else
            fprintf(1,'Passed ALL  %d tests in test_1_experiment\n',totalTests);
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