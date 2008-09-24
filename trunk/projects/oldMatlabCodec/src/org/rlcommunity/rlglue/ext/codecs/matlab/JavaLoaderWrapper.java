/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.rlcommunity.rlglue.ext.codecs.matlab;

import rlglue.agent.AgentLoader;
import rlglue.environment.EnvironmentLoader;

/**
 *
 * @author Brian Tanner
 */
public class JavaLoaderWrapper{
    AgentLoader theAgentLoader=null;
    EnvironmentLoader theEnvLoader=null;
    public JavaLoaderWrapper(MatlabAgentCodec theAgent) {
       theAgentLoader=new AgentLoader(theAgent); 
       
       Thread t=new Thread(theAgentLoader);
       t.start();
    }
    public JavaLoaderWrapper(MatlabEnvironmentCodec theEnv) {
       theEnvLoader = new EnvironmentLoader(theEnv); 
       
       Thread t=new Thread(theEnvLoader);
       t.start();
    }
    public void killThread(){
        theAgentLoader.killProcess();
    }


}
