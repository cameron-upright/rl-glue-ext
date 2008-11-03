/*
 * Copyright 2008 Brian Tanner
 * http://rl-glue-ext.ext.googlecode.com/
 * brian@tannerpages.com
 * http://brian.tannerpages.com
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.rlcommunity.rlglue.codec.tests;

import org.rlcommunity.rlglue.codec.RLGlue;
import org.rlcommunity.rlglue.codec.types.Random_seed_key;
import org.rlcommunity.rlglue.codec.types.State_key;

/**
 *
 * @author Brian Tanner
 */
public class Test_Seeds_Experiment {
    
public static int runTest(){
            Glue_Test tester=new Glue_Test("Test_Seeds_Experiment");

	State_key the_state_key=new State_key();
	State_key empty_state_key=new State_key();
	Random_seed_key the_random_seed=new Random_seed_key();
	Random_seed_key empty_random_seed=new Random_seed_key();
	
	State_key returned_state_key;
	Random_seed_key returned_random_seed_key;
	
	TestUtility.clean_abstract_type(the_state_key);
	TestUtility.clean_abstract_type(the_random_seed);
	TestUtility.clean_abstract_type(empty_state_key);
	TestUtility.clean_abstract_type(empty_random_seed);
	
	TestUtility.set_k_ints_in_abstract_type(the_state_key,3);
	TestUtility.set_k_doubles_in_abstract_type(the_state_key,7);
	TestUtility.set_k_chars_in_abstract_type(the_state_key,2);

	TestUtility.set_k_ints_in_abstract_type(the_random_seed,1);
	TestUtility.set_k_doubles_in_abstract_type(the_random_seed,2);
	TestUtility.set_k_chars_in_abstract_type(the_random_seed,4);
	
	/*	compare_abstract_types */
	
	RLGlue.RL_init();
	
	RLGlue.RL_load_state(the_state_key);
	returned_state_key=RLGlue.RL_save_state();
	tester.check_fail(the_state_key.compareTo(returned_state_key)!=0);

	RLGlue.RL_load_random_seed(the_random_seed);
	returned_random_seed_key=RLGlue.RL_save_random_seed();
	tester.check_fail(the_random_seed.compareTo(returned_random_seed_key)!=0);
	
	
	TestUtility.set_k_ints_in_abstract_type(the_state_key,0);
	TestUtility.set_k_doubles_in_abstract_type(the_state_key,0);
	TestUtility.set_k_chars_in_abstract_type(the_state_key,0);
	
	TestUtility.set_k_ints_in_abstract_type(the_random_seed,0);
	TestUtility.set_k_doubles_in_abstract_type(the_random_seed,0);
	TestUtility.set_k_chars_in_abstract_type(the_random_seed,0);

	RLGlue.RL_load_state(the_state_key);
	returned_state_key=RLGlue.RL_save_state();
	tester.check_fail(the_state_key.compareTo(returned_state_key)!=0);

	RLGlue.RL_load_random_seed(the_random_seed);
	returned_random_seed_key=RLGlue.RL_save_random_seed();
	tester.check_fail(the_random_seed.compareTo(returned_random_seed_key)!=0);
	
	/* Make sure if we send an empty we get back an empty */
	RLGlue.RL_load_state(empty_state_key);
	returned_state_key=RLGlue.RL_save_state();
	tester.check_fail(empty_state_key.compareTo(returned_state_key)!=0);

	RLGlue.RL_load_random_seed(empty_random_seed);
	returned_random_seed_key=RLGlue.RL_save_random_seed();
	tester.check_fail(empty_random_seed.compareTo(returned_random_seed_key)!=0);

	
        
        System.out.println(tester);
        return tester.getFailCount();
}
        

        public static void main(String[] args){

        System.exit(runTest());
    }

}
