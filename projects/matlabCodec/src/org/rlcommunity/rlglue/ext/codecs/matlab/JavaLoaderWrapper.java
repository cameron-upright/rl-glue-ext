/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.rlcommunity.rlglue.ext.codecs.matlab;

import rlglue.agent.AgentLoader;

/**
 *
 * @author Brian Tanner
 */
public class JavaLoaderWrapper{
    AgentLoader theAgentLoader=null;
    public JavaLoaderWrapper(MatlabAgentCodec theAgent) {
       theAgentLoader=new AgentLoader(theAgent); 
       
       Thread t=new Thread(theAgentLoader);
       t.start();
    }
    
    public void killThread(){
        theAgentLoader.killProcess();
    }


}
