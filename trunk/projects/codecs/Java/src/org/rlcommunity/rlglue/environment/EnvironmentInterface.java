/* 
* Copyright (C) 2007, Brian Tanner
* 
http://rl-glue.googlecode.com/

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
* 
*  $Revision: 120 $
*  $Date: 2008-09-12 23:32:53 -0600 (Fri, 12 Sep 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: https://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/rlglue/environment/EnvironmentInterface.java $
* 
*/


package org.rlcommunity.rlglue.environment;

import org.rlcommunity.rlglue.types.Action;
import org.rlcommunity.rlglue.types.Observation;
import org.rlcommunity.rlglue.types.Random_seed_key;
import org.rlcommunity.rlglue.types.Reward_observation;
import org.rlcommunity.rlglue.types.State_key;

public interface EnvironmentInterface
{
    String env_init();
    Observation env_start();
    Reward_observation env_step(Action action);
    void env_cleanup();
    void env_set_state(State_key key);
    void env_set_random_seed(Random_seed_key key);
    State_key env_get_state();
    Random_seed_key env_get_random_seed();
    String env_message(final String message);
}
