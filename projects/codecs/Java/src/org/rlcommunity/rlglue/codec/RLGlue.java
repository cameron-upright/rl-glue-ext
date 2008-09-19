/*
Copyright 2007 Brian Tanner
brian@tannerpages.com
http://brian.tannerpages.com

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

  
package org.rlcommunity.rlglue.codec;

import org.rlcommunity.rlglue.codec.types.Observation_action;
import org.rlcommunity.rlglue.codec.types.Random_seed_key;
import org.rlcommunity.rlglue.codec.types.Reward_observation_action_terminal;
import org.rlcommunity.rlglue.codec.types.State_key;

public class RLGlue{
	private static RLGlueInterface instance = null;
	static boolean inited=false;

	static boolean currentEpisodeOver=true;

        
        /**
         * Set this if you'd like to run an alternate glue engine
         * @param theGlueInstance
         */
        static public void setGlue(RLGlueInterface theGlueInstance){
            resetGlueProxy();
            instance=theGlueInstance;
        }

        static public void resetGlueProxy(){
            instance=null;
            inited=false;
            currentEpisodeOver=true;
        }
        
	static public boolean isInited(){
		return inited;
	}

        /**
         *  Exists only to defeat instantiation.
         */
        protected RLGlue() {
	}


        private static void checkInstance(){
		if(instance == null) {
                    //This isn't really an error
			instance = new NetGlue();
		}
	}

	public static String RL_agent_message(String message) {
		checkInstance();
		return instance.RL_agent_message(message);
	}
	public static  void RL_cleanup() {
		checkInstance();
                if(!inited)System.err.println("-- Warning From RLGlue :: RL_cleanup() was called without matching RL_init() call previously.");
                instance.RL_cleanup();
		inited=false;
	}
	public static String RL_env_message(String message) {
		checkInstance();
		return instance.RL_env_message(message);
	}
	public static int RL_episode(int numSteps) {
		checkInstance();
		int terminationCondition=instance.RL_episode(numSteps);
                return terminationCondition;
	}

	public static Random_seed_key RL_get_random_seed() {
		checkInstance();
		return instance.RL_get_random_seed();
	}
	public static State_key RL_get_state() {
		checkInstance();
		return instance.RL_get_state();
	}
	public static String RL_init() {
		checkInstance();
                if(inited)System.err.println("-- Warning From RLGlue :: RL_init() was called more than once without RL_cleanup.");
		String taskSpec=instance.RL_init();
		inited=true;
                currentEpisodeOver=true;
                return taskSpec;
	}
	public static int RL_num_episodes() {
		checkInstance();
		return instance.RL_num_episodes();
	}

	public static int RL_num_steps() {
		checkInstance();
		return instance.RL_num_steps(); 
	}
	public static double RL_return() {
		checkInstance();
		return instance.RL_return();
	}
	public static void RL_set_random_seed(Random_seed_key rsk) {
		checkInstance();
		instance.RL_set_random_seed(rsk);
	}
	public static void RL_set_state(State_key sk) {
		checkInstance();
		instance.RL_set_state(sk);
	}
	
	public static Observation_action RL_start() {
		checkInstance();
                if(!inited)System.err.println("-- Warning From RLGlue :: RL_start() was called without RL_init().");
                currentEpisodeOver=false;
		return instance.RL_start();
	}
	public static Reward_observation_action_terminal RL_step() {
		checkInstance();
                if(!inited)System.err.println("-- Warning From RLGlue :: RL_step() was called without RL_init().");
                Reward_observation_action_terminal stepResponse=instance.RL_step();
                currentEpisodeOver=(stepResponse.terminal==1);

		return stepResponse;
	}

        public static boolean isCurrentEpisodeOver(){
            return currentEpisodeOver;
        }
}
