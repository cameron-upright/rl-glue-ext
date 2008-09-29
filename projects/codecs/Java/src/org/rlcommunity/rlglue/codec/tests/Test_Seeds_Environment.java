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

import org.rlcommunity.rlglue.codec.EnvironmentInterface;
import org.rlcommunity.rlglue.codec.util.EnvironmentLoader;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.Random_seed_key;
import org.rlcommunity.rlglue.codec.types.Reward_observation;
import org.rlcommunity.rlglue.codec.types.State_key;

/**
 *
 * @author Brian Tanner
 */
public class Test_Seeds_Environment implements EnvironmentInterface {

    Observation o =new Observation();
    
    State_key savedStateKey=new State_key();
    Random_seed_key savedRandomSeed = new Random_seed_key();
    
    
    public static void main(String[] args){
        EnvironmentLoader L=new EnvironmentLoader(new Test_Seeds_Environment());
        L.run();
    }

        
    public Test_Seeds_Environment() {
    }

    

    public String env_init() {
	return "";    }

    public Observation env_start() {
        TestUtility.clean_abstract_type(o);
        return o;   
    }

    public Reward_observation env_step(Action action) {
        TestUtility.clean_abstract_type(o);
        int terminal=0;
        Reward_observation ro=new Reward_observation(0.0d, o, terminal);
        return ro;
    }

    public void env_cleanup() {
    }

    public void env_set_state(State_key key) {
        this.savedStateKey=key;
    }

    public void env_set_random_seed(Random_seed_key key) {
        this.savedRandomSeed=key;
    }

    public State_key env_get_state() {
        return savedStateKey;
    }

    public Random_seed_key env_get_random_seed() {
        return savedRandomSeed;
    }

    public String env_message(String message) {
        return "";
    }
}
