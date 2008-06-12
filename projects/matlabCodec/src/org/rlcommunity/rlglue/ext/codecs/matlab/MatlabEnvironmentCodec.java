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
    static private int freezeNum = 3;
    static private int cleanupNum = 4;
    static private int messageNum = 5;
    static private int setstateNum = 6;
    static private int getstateNum = 7;
    static private int setrandomseedNum = 8;
    static private int getrandomseedNum = 9;

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
        String taskspec = "";
        try {

            taskspec = (String) mc.blockingFeval(env_initFunc, null);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabEnvironmentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        return taskspec;
    }

    public Observation env_start() {
        Observation theObservation = null;
        try {
            Object returnObject = mc.blockingFeval(env_startFunc, null);
            Object[] roa=(Object[])returnObject;                 
            int[] intPart =(int[])roa[0];
            double[] doublePart = (double[])roa[1];        
            
            theObservation=new Observation(intPart.length,doublePart.length);
            theObservation.intArray=intPart;
            theObservation.doubleArray=doublePart;
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabEnvironmentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        return theObservation;
    }

    public Reward_observation env_step(Action action) {
        Reward_observation rewardObs = null;
        try {
            Object[] args = new Object[2];
            args[0] = action.intArray;
            args[1] = action.doubleArray;
            Object returnObject = mc.blockingFeval(env_stepFunc, args);
            Object[] roa=(Object[])returnObject;                 
            int[] intPart =(int[])roa[0];
            double[] doublePart = (double[])roa[1];         
            double[] rewardPart = (double[])roa[2];
            double[] terminalPart = (double[])roa[3];
            double theReward = rewardPart[0];
            double terminal = terminalPart[0];
            Observation theObs = new Observation(intPart.length,doublePart.length);
            theObs.intArray=intPart;
            theObs.doubleArray=doublePart;
            rewardObs =new Reward_observation(theReward, theObs,(int) terminal);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabAgentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }

        return rewardObs;
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
        State_key sk = null;
        try {
            Object returnObject = mc.blockingFeval(env_getstateFunc, null);
            Object[] roa=(Object[])returnObject;                 
            int[] intPart =(int[])roa[0];
            double[] doublePart = (double[])roa[1];
            sk = new State_key(intPart.length,doublePart.length);
            sk.doubleArray = doublePart;
            sk.intArray = intPart;
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabEnvironmentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        return sk;
    }

    public Random_seed_key env_get_random_seed() {
        Random_seed_key rsk = null;
        try {
            Object returnObject = mc.blockingFeval(env_getrandomseedFunc, null);
            Object[] roa=(Object[])returnObject;                 
            int[] intPart =(int[])roa[0];
            double[] doublePart = (double[])roa[1];
            rsk = new Random_seed_key(intPart.length,doublePart.length);
            rsk.doubleArray = doublePart;
            rsk.intArray = intPart;

        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabEnvironmentCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        return rsk;
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
