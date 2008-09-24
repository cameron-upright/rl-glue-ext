/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.rlcommunity.rlglue.ext.codecs.matlab;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Brian Tanner
 */
public class SketchMaster implements Runnable{
    private static int nextNumber=-1;
    int someNumber;

    public SketchMaster(){
        nextNumber++;
        this.someNumber=nextNumber;
    }

    public void run() {
        try {
            System.out.println("SketchMaster: " + someNumber + " started.. sleeping for a while");
            Thread.sleep(10000);
        } catch (InterruptedException ex) {
            Logger.getLogger(SketchMaster.class.getName()).log(Level.SEVERE, null, ex);
        }
            System.out.println("SketchMaster: " + someNumber + " all done...");
    }
}
