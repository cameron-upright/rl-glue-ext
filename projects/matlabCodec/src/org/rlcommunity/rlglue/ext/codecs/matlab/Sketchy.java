/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.rlcommunity.rlglue.ext.codecs.matlab;

import java.util.LinkedList;
import java.util.Queue;
import java.util.logging.Level;
import java.util.logging.Logger;
import rlglue.types.Action;
import rlglue.types.Observation;

/**
 *
 * @author Brian Tanner
 */
public class Sketchy extends Thread{
    volatile public boolean DIE=false;
MatlabCodec theAgent=null;

    public Sketchy(MatlabCodec theAgent) {
        this.theAgent=theAgent;
        start();
    }
    
    @Override
    public void run(){
        System.out.println("Running: "+this.getClass());
        while(!DIE){

            if(Math.random()<.1){
                //Pretend we heard agent_step
                Action theAction=theAgent.agent_step(5, new Observation(1,0));
                //Return the action out over the network
            }
            try {
                //Pretend we're on the network
                Thread.sleep(100);
            } catch (InterruptedException ex) {
                Logger.getLogger(Sketchy.class.getName()).log(Level.SEVERE, null, ex);
            }
            
        }
        System.out.println(this.getClass()+" dieing");
    }

}
