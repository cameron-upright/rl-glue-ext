/*
 * Copyright 2008 Brian Tanner
 * http://rl-glue-ext.googlecode.com/
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


This code is adapted from the Mines.cpp code written by Adam White
for earlier versions of RL-Glue.

 *  $Revision: 821 $
 *  $Date: 2008-09-14 14:21:41 -0600 (Sun, 14 Sep 2008) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: https://rl-glue.googlecode.com/svn/trunk/src/RL_direct_agent.c $

 *
 */

import java.util.Random;
import org.rlcommunity.rlglue.codec.EnvironmentInterface;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;
import org.rlcommunity.rlglue.codec.util.EnvironmentLoader;
import org.rlcommunity.rlglue.codec.taskspec.TaskSpecVRLGLUE3;
import org.rlcommunity.rlglue.codec.taskspec.TaskSpec;
import org.rlcommunity.rlglue.codec.taskspec.ranges.IntRange;
import org.rlcommunity.rlglue.codec.taskspec.ranges.DoubleRange;

/**
 *  This is a very simple environment with discrete observations corresponding to states labeled {0,1,...,19,20}
The starting state is 10.

There are 2 actions = {0,1}.  0 decrements the state, 1 increments the state.

The problem is episodic, ending when state 0 or 20 is reached, giving reward -1 or +1, respectively.  The reward is 0 on 
all other steps.
 * @author Brian Tanner
 */
public class SampleMinesEnvironment implements EnvironmentInterface {

    static final int WORLD_FREE = 0;
    static final int WORLD_OBSTACLE = 1;
    static final int WORLD_MINE = 2;
    static final int WORLD_GOAL = 3;
    int world_map[][] = new int[][]{
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
        {1, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1},
        {1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1},
        {1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 1, 1},
        {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
    };
    WorldDescription theWorld;

    public String env_init() {
        theWorld = new WorldDescription(world_map);


        //Create a task spec programatically.  This task spec encodes that state, action, and reward space for the problem.
        //You could forgo the task spec if your agent and environment have been created specifically to work with each other
        //ie, there is no need to share this information at run time.  You could also use your own ad-hoc task specification language,
        //or use the official one but just hard code the string instead of constructing it this way.
        TaskSpecVRLGLUE3 theTaskSpecObject = new TaskSpecVRLGLUE3();
        theTaskSpecObject.setEpisodic();
        theTaskSpecObject.setDiscountFactor(1.0d);

        //Specify that there will be an integer observation [0,108] for the state
        theTaskSpecObject.addDiscreteObservation(new IntRange(0, theWorld.getNumStates() - 1));
        //Specify that there will be an integer action [0,1]
        theTaskSpecObject.addDiscreteAction(new IntRange(0, 1));
        //Specify the reward range [-1,1]
        theTaskSpecObject.setRewardRange(new DoubleRange(-1, 1));

        String taskSpecString = theTaskSpecObject.toTaskSpec();
        TaskSpec.checkTaskSpec(taskSpecString);

        return taskSpecString;
    }

    public Observation env_start() {
        theWorld.setRandomAgentState();

        Observation returnObservation = new Observation(1, 0, 0);
        returnObservation.intArray[0] = currentState;
        return returnObservation;
    }

    public Reward_observation_terminal env_step(Action thisAction) {
        boolean episodeOver = false;
        double theReward = 0.0d;

        if (thisAction.intArray[0] == 0) {
            currentState--;
        }
        if (thisAction.intArray[0] == 1) {
            currentState++;
        }

        if (currentState <= 0) {
            currentState = 0;
            theReward = -1.0d;
            episodeOver = true;
        }

        if (currentState >= 20) {
            currentState = 20;
            episodeOver = true;
            theReward = 1.0d;
        }
        Observation returnObservation = new Observation(1, 0, 0);
        returnObservation.intArray[0] = currentState;

        Reward_observation_terminal returnRewardObs = new Reward_observation_terminal(theReward, returnObservation, episodeOver);
        return returnRewardObs;
    }

    public void env_cleanup() {
    }

    public String env_message(String message) {
        if (message.equals("what is your name?")) {
            return "my name is skeleton_environment, Java edition!";
        }

        return "I don't know how to respond to your message";
    }

    /**
     * This is a trick we can use to make the agent easily loadable.
     * @param args
     */
    public static void main(String[] args) {
        EnvironmentLoader theLoader = new EnvironmentLoader(new SampleMinesEnvironment());
        theLoader.run();
    }

    class WorldDescription {

        private final int numRows;
        private final int numCols;
        public int agentRow;
        public int agentCol;
        private final int[][] theMap;
        private Random randGen = new Random();

        private WorldDescription(int[][] worldMap) {
            this.theMap = worldMap;

            this.numRows = theMap.length;
            this.numCols = theMap[0].length;
        }

        private int getNumStates() {
            return numRows * numCols;
        }

        private void setRandomAgentState() {

            int startRow = randGen.nextInt(numRows);
            int startCol = randGen.nextInt(numCols);

            while (check_terminal(startRow, startCol) || !check_valid(startRow, startCol)) {
                startRow = randGen.nextInt(numRows);
                startCol = randGen.nextInt(numCols);
            }

            this.agentRow = startRow;
            this.agentCol = startCol;
        }

        private Observation makeObservation(){
            Observation theObservation=new Observation(1,0,0);
            theObservation.intArray[0] = calculate_flat_state();
            return theObservation;

        }

        int calculate_flat_state() {
            return agentCol * numRows + agentRow;
        }

        boolean check_terminal(int row, int col) {
            if (theMap[row][col] == WORLD_GOAL || theMap[row][col] == WORLD_MINE) {
                return true;
            }
            return false;
        }

        boolean check_valid(int row, int col) {
            boolean valid = false;
            if (row < numRows && row >= 0 && col < numCols && col >= 0) {
                if (world_map[row][col] != WORLD_OBSTACLE) {
                    valid = true;
                }
            }
            return valid;
        }
    }
}
