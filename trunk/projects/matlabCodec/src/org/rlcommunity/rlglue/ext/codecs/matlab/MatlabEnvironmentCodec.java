package org.rlcommunity.rlglue.ext.codecs.matlab;

import java.util.logging.Level;
import java.util.logging.Logger;
import rlglue.environment.Environment;
import rlglue.types.Action;
import rlglue.types.Observation;
import rlglue.types.Random_seed_key;
import rlglue.types.Reward_observation;
import rlglue.types.State_key;
import rlglue.utilities.TaskSpec;

/**
 *
 * @author mradkie
 */
public class MatlabEnvironmentCodec implements Environment {

    MatlabControl mc;// = new MatlabControl();
    //main for testing stuff.
    String env_initFunc;
    String env_startFunc;
    String env_stepFunc;
    String env_endFunc;
    String env_freezeFunc;
    String env_cleanupFunc;
    String env_messageFunc;
    String env_setstateFunc;
    String env_getstateFunc;
    String env_setrandomseedFunc;
    String env_getrandomseedFunc;
    
    static private int initNum = 0;
    static private int startNum = 1;
    static private int stepNum = 2;
    static private int endNum = 3;
    static private int freezeNum = 4;
    static private int cleanupNum = 5;
    static private int messageNum = 6;
    static private int setstateNum = 7;
    static private int getstateNum = 8;
    static private int setrandomseedNum = 9;
    static private int getrandomseedNum = 10;

    public static void main(String[] args) {

    }

    public void test() {
        mc.setEchoEval(true);
        mc.testBlockingFeval("mtest", null);
    }

    public MatlabEnvironmentCodec(String[] matLabEnvironmentCode) {
        if (mc == null) {
            mc = new MatlabControl();
        }
        //probably dont want all of these, but added them because we want to
        //provide full functionality.
        env_initFunc = matLabEnvironmentCode[initNum];
        env_startFunc = matLabEnvironmentCode[startNum];
        env_stepFunc = matLabEnvironmentCode[stepNum];
        env_endFunc = matLabEnvironmentCode[endNum];
        env_freezeFunc = matLabEnvironmentCode[freezeNum];
        env_cleanupFunc = matLabEnvironmentCode[cleanupNum];
        env_messageFunc = matLabEnvironmentCode[messageNum];
        env_setstateFunc = matLabEnvironmentCode[setstateNum];
        env_getstateFunc = matLabEnvironmentCode[getstateNum];
        env_setrandomseedFunc = matLabEnvironmentCode[setrandomseedNum];
        env_getrandomseedFunc = matLabEnvironmentCode[getrandomseedNum];
    }

    /**
     * This will be called from the Glue.
     */
    public String env_init() {
        try {
            return (String) mc.blockingFeval(env_messageFunc, null);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabEnvironmentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        return "";
    }

    public Observation env_start() {
        try {
            return (Observation) mc.blockingFeval(env_startFunc, null);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabEnvironmentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    public Reward_observation env_step(Action action) {
        try {
            Object[] args = new Object[2];
            args[0] = action.intArray;
            args[1] = action.doubleArray;
            return (Reward_observation) mc.blockingFeval(env_stepFunc, args);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabAgentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }

        return null;
    }

    public void env_set_state(State_key sk) {
        Object[] args = new Object[2];
        args[0] = sk.intArray;
        args[1] = sk.doubleArray;
        mc.testFeval(env_setstateFunc, args);
    }

    public void env_set_random_seed(Random_seed_key rsk) {
        Object[] args = new Object[2];
        args[0] = rsk.intArray;
        args[1] = rsk.doubleArray;
        mc.testFeval(env_setrandomseedFunc, args);
    }

    public State_key env_get_state() {
        try {
            return (State_key) mc.blockingFeval(env_getstateFunc, null);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabEnvironmentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    public Random_seed_key env_get_random_seed() {
        try {
            return (Random_seed_key) mc.blockingFeval(env_getrandomseedFunc, null);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabEnvironmentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    public void env_cleanup() {
        mc.testEval(env_cleanupFunc);
    }

    public String env_message(String message) {
        String returnMessage = "";
        try {
            Object[] args = new Object[1];
            args[0] = message;
            returnMessage = (String) mc.blockingFeval(env_messageFunc, args);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabAgentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }

        return returnMessage;
    }
}
