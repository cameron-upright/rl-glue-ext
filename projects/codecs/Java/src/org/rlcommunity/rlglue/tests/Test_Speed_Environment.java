/*
 * Copyright 2008 Brian Tanner
 * http://bt-recordbook.googlecode.com/
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
package org.rlcommunity.rlglue.tests;

import org.rlcommunity.rlglue.environment.EnvironmentInterface;
import org.rlcommunity.rlglue.environment.EnvironmentLoader;
import org.rlcommunity.rlglue.types.Action;
import org.rlcommunity.rlglue.types.Observation;
import org.rlcommunity.rlglue.types.Random_seed_key;
import org.rlcommunity.rlglue.types.Reward_observation;
import org.rlcommunity.rlglue.types.State_key;

/**
 *
 * @author Brian Tanner
 */
public class Test_Speed_Environment implements EnvironmentInterface {


   int whichEpisode=0;
   int stepCount=0;
   
   Observation o =new Observation();
    
    public Test_Speed_Environment() {
    }

    
    public String env_message(String inMessage) {
        return "";
    }

    public static void main(String[] args){
        EnvironmentLoader L=new EnvironmentLoader(new Test_Speed_Environment());
        L.run();
    }

    public String env_init() {
        return "";
    }
    public Observation env_start() {
        stepCount=0;
        whichEpisode++;
        TestUtility.clean_abstract_type(o);
        return o;   
    }

    public Reward_observation env_step(Action action) {
        stepCount++;
        Reward_observation ro=null;
        
        TestUtility.clean_abstract_type(o);
        
        //Short episode with big observations
        if(whichEpisode%2==0){
            TestUtility.set_k_ints_in_abstract_type(o, 50000);
            TestUtility.set_k_doubles_in_abstract_type(o, 50000);

            int terminal=0;
            if(stepCount==200)terminal=1;
                ro=new Reward_observation(1.0d, o, terminal);
        }
        //Longer episode with smaller obserations
        if(whichEpisode%2==1){
            TestUtility.set_k_ints_in_abstract_type(o, 5);
            TestUtility.set_k_doubles_in_abstract_type(o, 5);

            int terminal=0;
            if(stepCount==5000)terminal=1;
                ro=new Reward_observation(1.0d, o, terminal);
        }
        
                
        return ro;
    }

    public void env_cleanup() {
    }

    public void env_set_state(State_key key) {
    }

    public void env_set_random_seed(Random_seed_key key) {
    }

    public State_key env_get_state() {
        return new State_key();
    }

    public Random_seed_key env_get_random_seed() {
        return new Random_seed_key();
    }

}
