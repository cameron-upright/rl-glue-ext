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
    public JavaLoaderWrapper(MatlabAgentCodec theAgent) {
       AgentLoader theAgentLoader=new AgentLoader(theAgent); 
       
       Thread t=new Thread(theAgentLoader);
       t.start();
    }


}
