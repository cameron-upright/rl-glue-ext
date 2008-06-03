package org.rlcommunity.rlglue.ext.codecs.matlab;

/* Agent Interface 
void agent_init(const Task_specification task_spec);
Action agent_start(Observation o);
Action agent_step(Reward r, Observation o);
void agent_end(Reward r);  
void agent_cleanup();
void agent_freeze();
Message agent_message(const Message message);
*/
import java.util.logging.Level;
import java.util.logging.Logger;
import rlglue.agent.Agent;
import rlglue.types.Action;
import rlglue.types.Observation;
import rlglue.utilities.TaskSpec;
/**
 *
 * @author mradkie
 */

/*
 *  MatlabControl.eval(String command)
    MatlabControl.feval(String command, Object[] args)
    Object[] MatlabControl.blockingFeval(String command, Object[] args)
 * */

public class MatlabCodec implements Agent {
    MatlabControl mc;// = new MatlabControl();
    //main for testing stuff.
    
    String agent_initFunc;
    String agent_startFunc;
    String agent_stepFunc;
    String agent_endFunc;
    
    static private int initNum=0;
    static private int startNum=1;
    static private int stepNum=2;
    static private int endNum=3;
    
    
    public static void main(String[] args){
        
    }
    
    public MatlabCodec(String[] matLabAgentCode){
        if(mc == null) mc = new MatlabControl();
        
         agent_initFunc=matLabAgentCode[initNum];
         agent_startFunc=matLabAgentCode[startNum];
         agent_stepFunc=matLabAgentCode[stepNum];
         agent_endFunc=matLabAgentCode[endNum];
    }
    
    /**
     * This will be called from the Glue
     * @param task_spec
     */public void agent_init(String task_spec){
        mc.testFeval(agent_initFunc, new Object[]{task_spec});
    }

    public Action agent_start(Observation obs) {
        Object returnObj = new Object[2];
        try {
            Object[] args = new Object[2];
            args[0] = obs.intArray;
            args[1] = obs.doubleArray;
            returnObj=mc.blockingFeval(agent_startFunc, args);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return null;
    }

    public Action agent_step(double reward, Observation obs) {
        Object returnObj = new Object[2];
        try {
            Object[] args = new Object[3];
            args[0] = obs.intArray;
            args[1] = obs.doubleArray;
            args[2] = reward;
            returnObj=mc.blockingFeval(agent_startFunc, args);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return null;
    }

    public void agent_end(double reward) {
        mc.testFeval(agent_initFunc, new Object[]{reward});
    }

    public void agent_cleanup() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void agent_freeze() {
        mc.eval(new String("agent_freeze"));
    }

    public String agent_message(String arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
