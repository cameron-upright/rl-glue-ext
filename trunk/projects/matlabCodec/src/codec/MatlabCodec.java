package codec;

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
import rlVizLib.utilities.TaskSpecObject;
import rlglue.agent.Agent;
import rlglue.types.Action;
import rlglue.types.Observation;
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
    public static void main(String[] args){
        
    }
    
    public MatlabCodec(MatLabAgentStruct matLabAgentCode){
        
        if(mc == null) mc = new MatlabControl();
        
    }
    
    public void agent_init(TaskSpecObject task_spec){
        String theTaskSpec = task_spec.toString();
        agent_init(theTaskSpec);
    }
    
    public void agent_init(String task_spec){
        Object[] args = new Object[1];
        args[0]=task_spec;
        
        mc.feval(new String("agent_init"), args);
    }

    public Action agent_start(Observation obs) {
        Object[] returnObj = new Object[2];
        try {
            Object[] args = new Object[2];
            args[0] = obs.intArray;
            args[1] = obs.doubleArray;
            mc.blockingFeval(new String("agent_start"), args);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabCodec.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return null;
    }

    public Action agent_step(double arg0, Observation arg1) {
        return null;
    }

    public void agent_end(double arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
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
