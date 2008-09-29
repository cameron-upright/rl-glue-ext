/*
 * Copyright 2008 Brian Tanner
 * http://rl-glue.ext.googlecode.com/
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
import org.rlcommunity.rlglue.codec.types.Reward_observation_action_terminal;

/**
 *
 * @author Brian Tanner
 */
public class Test_1_Experiment {
    public static int runTest(){
        Glue_Test tester=new Glue_Test("Test_1_Experiment");
        Reward_observation_action_terminal roat;

        String task_spec=RLGlue.RL_init();

	RLGlue.RL_start();

        	roat=RLGlue.RL_step();

	
	
	tester.check_fail(roat.o.intArray.length!=1);
	tester.check_fail(roat.o.doubleArray.length!=0);
	tester.check_fail(roat.o.charArray.length!=0);
	tester.check_fail(roat.o.intArray[0]!=0);
        tester.check_fail(!"one|1.|one".equals(RLGlue.RL_env_message("one")));
        tester.check_fail(!"one|1.|one".equals(RLGlue.RL_agent_message("one")));

	tester.check_fail(roat.terminal!=0);
	

	roat=RLGlue.RL_step();

        tester.check_fail(!"two|2.2.|two".equals(RLGlue.RL_env_message("two")));
        tester.check_fail(!"two|2.2.|two".equals(RLGlue.RL_agent_message("two")));
	tester.check_fail(roat.terminal!=0);
	tester.check_fail(roat.o.intArray.length!=1);
	tester.check_fail(roat.o.doubleArray.length!=0);
	tester.check_fail(roat.o.charArray.length!=0);
	tester.check_fail(roat.o.intArray[0]!=1);

	roat=RLGlue.RL_step();

        tester.check_fail(!"three||three".equals(RLGlue.RL_env_message("three")));
        tester.check_fail(!"three||three".equals(RLGlue.RL_agent_message("three")));
	tester.check_fail(roat.terminal!=0);
	tester.check_fail(roat.o.intArray.length!=1);
	tester.check_fail(roat.o.doubleArray.length!=0);
	tester.check_fail(roat.o.charArray.length!=0);	
	tester.check_fail(roat.o.intArray[0]!=2);

	roat=RLGlue.RL_step();
        tester.check_fail(!"four|4.|four".equals(RLGlue.RL_env_message("four")));
        tester.check_fail(!"four|4.|four".equals(RLGlue.RL_agent_message("four")));
	tester.check_fail(roat.terminal!=0);
	tester.check_fail(roat.o.intArray.length!=1);
	tester.check_fail(roat.o.doubleArray.length!=0);
	tester.check_fail(roat.o.charArray.length!=0);
	tester.check_fail(roat.o.intArray[0]!=3);
	

	roat=RLGlue.RL_step();
        tester.check_fail(!"five|5.5.|five".equals(RLGlue.RL_env_message("five")));
        tester.check_fail(!"five|4.|five".equals(RLGlue.RL_agent_message("five")));
	tester.check_fail(roat.terminal==0);
        
        
        
        System.out.println(tester);
        
        return tester.getFailCount();
    }
    public static void main(String[] args){
        System.exit(runTest());
    }

}
