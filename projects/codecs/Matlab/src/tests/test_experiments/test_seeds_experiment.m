function failures=test_seeds_experiment()
        failures=0;
        totalTests=0;
        
        
        
	the_state_key=org.rlcommunity.rlglue.codec.types.State_key;
	empty_state_key=org.rlcommunity.rlglue.codec.types.State_key;
	the_random_seed=org.rlcommunity.rlglue.codec.types.Random_seed_key;
	empty_random_seed=org.rlcommunity.rlglue.codec.types.Random_seed_key;
	
	
	org.rlcommunity.rlglue.codec.tests.TestUtility.clean_abstract_type(the_state_key);
	org.rlcommunity.rlglue.codec.tests.TestUtility.clean_abstract_type(the_random_seed);
	org.rlcommunity.rlglue.codec.tests.TestUtility.clean_abstract_type(empty_state_key);
	org.rlcommunity.rlglue.codec.tests.TestUtility.clean_abstract_type(empty_random_seed);
	
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_ints_in_abstract_type(the_state_key,3);
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_doubles_in_abstract_type(the_state_key,7);
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_chars_in_abstract_type(the_state_key,2);

	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_ints_in_abstract_type(the_random_seed,1);
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_doubles_in_abstract_type(the_random_seed,2);
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_chars_in_abstract_type(the_random_seed,4);
	
	%/*	compare_abstract_types */
	
	RL_init();
	
	RL_set_state(the_state_key);
	returned_state_key=RL_get_state();
	[failures,totalTests]=check_fail(the_state_key.compareTo(returned_state_key)~=0,failures,totalTests);

	RL_set_random_seed(the_random_seed);
	returned_random_seed_key=RL_get_random_seed();
	[failures,totalTests]=check_fail(the_random_seed.compareTo(returned_random_seed_key)~=0,failures,totalTests);
	
	
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_ints_in_abstract_type(the_state_key,0);
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_doubles_in_abstract_type(the_state_key,0);
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_chars_in_abstract_type(the_state_key,0);
	
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_ints_in_abstract_type(the_random_seed,0);
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_doubles_in_abstract_type(the_random_seed,0);
	org.rlcommunity.rlglue.codec.tests.TestUtility.set_k_chars_in_abstract_type(the_random_seed,0);

	RL_set_state(the_state_key);
	returned_state_key=RL_get_state();
	[failures,totalTests]=check_fail(the_state_key.compareTo(returned_state_key)~=0,failures,totalTests);

	RL_set_random_seed(the_random_seed);
	returned_random_seed_key=RL_get_random_seed();
	[failures,totalTests]=check_fail(the_random_seed.compareTo(returned_random_seed_key)~=0,failures,totalTests);
	
	%/* Make sure if we send an empty we get back an empty */
	RL_set_state(empty_state_key);
	returned_state_key=RL_get_state();
	[failures,totalTests]=check_fail(empty_state_key.compareTo(returned_state_key)~=0,failures,totalTests);

	RL_set_random_seed(empty_random_seed);
	returned_random_seed_key=RL_get_random_seed();
	[failures,totalTests]=check_fail(empty_random_seed.compareTo(returned_random_seed_key)~=0,failures,totalTests);
    
    
    


            
        if(failures>0)
            fprintf(1,'Failed %d of %d tests in test_seeds_experiment\n',failures,totalTests);
        else
            fprintf(1,'Passed ALL  %d tests in test_seeds_experiment\n',totalTests);
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