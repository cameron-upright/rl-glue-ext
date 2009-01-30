/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.rlcommunity.rlglue.codec.installer;

import org.rlcommunity.rlglue.codec.RLGlueCore;

/**
 * This is meant to be used from the command line to see if RL-Glue is installed.
 *
 * So you could type at the console:
 * >$ org.rlcommunity.rlglue.codec.installer.isInstalled
 *
 * And you would see a printout like:
 * @author Brian Tanner
 */
public class isInstalled {

    public static void main(String[] args){
        System.out.println(RLGlueCore.getImplementationVersion());
    }
}
