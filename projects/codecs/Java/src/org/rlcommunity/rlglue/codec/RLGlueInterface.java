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

/**
 * This is the definition of an RLGlue 'engine'.  The network codec is one such engine.
 * <p>If you want to create your own network codec, or a 'direct-compile' RL-Glue 
 * engine (like the one in RL-Viz), you should implement this interface.
 * @since 2.0
 * @author btanner
 */
public interface RLGlueInterface {
	public   String RL_init();
	public   Observation_action RL_start();
	public   Reward_observation_action_terminal RL_step();
	public   void RL_cleanup();
	public   String RL_agent_message(String message);
	public   String RL_env_message(String message);
	public   double RL_return();
	public   int RL_num_steps();
	public   int RL_num_episodes();
	public   int RL_episode(int numSteps);
	public   void RL_load_state(State_key stateKey);
	public   void RL_load_random_seed(Random_seed_key randomSeedKey);
	public   State_key RL_save_state();
	public   Random_seed_key RL_save_random_seed();
}
