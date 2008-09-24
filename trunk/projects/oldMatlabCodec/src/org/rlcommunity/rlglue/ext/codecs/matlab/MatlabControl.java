package org.rlcommunity.rlglue.ext.codecs.matlab;
/*
 *  This code originally came from:
 *  http://www.cs.virginia.edu/~whitehouse/matlab/MatlabControl.java 
 * 
 *  Brian Tanner and Matt Radkie are changing it to work more like what 
 *  we need for our RL-Glue Matlab integration.
 * 
 */
/* MatlabControl.java
 *
 * "Copyright (c) 2001 and The Regents of the University
 * of California.  All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * $\Id$
 */

/**
 * This class interfaces with the current Matlab session, allowing you
 * to call matlab commands from Java objects
 *
 * @author <a href="mailto:kamin@cs.berkeley.edu">Kamin Whitehouse</a>
 *///package net.tinyos.matlab;
import java.lang.System.*;

import com.mathworks.jmi.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public class MatlabControl {
    Matlab matlab = null; //this is the com.mathworks.jmi.Matlab class,which has functionality allowing one to interact with the matlab session.
    boolean useCb = false;
    Object returnVal;
    String callbackFunction;

    /***************CONSTRUCTORS****************/
    /*** usually, the default constructor with no arguments is fine.
     * Sometimes, a callback function is useful.  A callback function
     * is allows the user to pass the command and its arguments to a
     * matlab function, which will figure out the best way to executue
     * it, instead of trying to execute it directly from java.  This
     * is often useful for handling/printing errors as well as dealing
     * with native matlab data types, like cell arrays.  The cell
     * array is not really converted to a java type, only the handle
     * of the cell array is converted to a java type, so often a
     * matlab callback function is useful for dealing specially with
     * cell arrays, etc.
     ***/
    public MatlabControl() {
        this(false);
    }
    
    public MatlabControl(boolean useCallback) {
        this(useCallback, new String("matlabControlcb"));
    }

    public MatlabControl(boolean useCallback, String CallBackFunction) {
        try {
            if (matlab == null) {
                matlab = new Matlab();//this command links to the current matlab session
            }
        } catch (Exception e) {
            System.out.println(e.toString());
        }
        returnVal = new String("noReturnValYet");
        this.useCb = useCallback;
        callbackFunction = CallBackFunction;
    }

    /***************USER-LEVEL FUNCTIONS****************/
    /***call these from any java thread (as long as the thread
     * originated from within matlab) 
     ***/
    /**Evaluate a string, Matlab script, or Matlab function**/
    public void eval(String Command) {
        Matlab.whenMatlabReady(new MatlabEvalCommand(Command, useCb));
    }

    /**Evaluate a Matlab function that requires arguments.  Each element of
    the "args" vector is an argument to the function "Command"**/
    public void feval(String Command, Object[] args) {
        Matlab.whenMatlabReady(new MatlabFevalCommand(Command, args, useCb));
    }

    /**Evaluate a Matlab function that requires arguments and provide return arg.
     * Each element of the "args" vector is an argument to the function "Command"**/
    public Object blockingFeval(String Command, Object[] args) throws InterruptedException {
        returnVal = new String("noReturnValYet");
        Matlab.whenMatlabReady(new MatlabBlockingFevalCommand(Command, args, useCb, this));
        if (returnVal.equals(new String("noReturnValYet"))) {
            synchronized (returnVal) {
                returnVal.wait();
            }
        }
        return returnVal;
    }

    /**Echoing the eval statement is useful if you want to see in
     * matlab each time that a java function tries to execute a matlab
     * command **/
    public void setEchoEval(boolean echo) {
        Matlab.setEchoEval(echo);
    }

    /**********TEST FUNCTIONS***********************/
    /***call these functions from within Matlab itself.  These are examples of the general execution order:
    1. instantiate java object from matlab
    2. spawn a new Java thread
    3. call matlab functions from new java thread  
    
    EXAMPLE (from matlab prompt):
    
    >> mc=MatlabControl;
    >> mc.testEval('x = 5')
    x =
    5
    > mc.testFeval('help',{'sqrt'})
    SQRT    Square root.
    SQRT(X) is the square root of the elements of X. Complex
    results are produced if X is not positive.
    
    See also SQRTM.
    
    Overloaded methods
    help sym/sqrt.m
    
    >> mc.testBlockingFeval('sqrt',{x})
    2.2361
     ****/
    public void testEval(final String Command) {
        class Caller extends Thread {

            public void run() {
                try {
                    eval(Command);
                } catch (Exception e) {
                    //                System.out.println(e.toString());
                }
            }
        }
        Caller c = new Caller();
        c.start();
    }

    public void testFeval(final String Command, final Object[] args) {
        class Caller extends Thread {

            public void run() {
                try {
                    feval(Command, args);
                } catch (Exception e) {
                    //                System.out.println(e.toString());
                }
            }
        }
        Caller c = new Caller();
        c.start();
    }

    public Object testBlockingFeval(final String Command, final Object[] args) {
        class Caller implements Runnable {

            Object returnValue = null;

            public void run() {
                try {
            System.out.println("\t TBF thread: "/*+Thread.currentThread().getId()*/+" active count is: "+Thread.activeCount());
                    System.out.println("TBF running in its thread");
//                    returnValue = blockingFeval(Command, args);
//                    Object rets[] = new Object[1];
//                    rets[0] = blockingFeval(Command, args);
//                    
//                    if(returnValue==null)System.out.println("in TBF: returnValue is null");
//                    if(rets[0]==null)System.out.println("in TBF: rets[0] is null");
//                    System.out.println("Back from MC we got: " + rets[0] + " of type: " + rets[0].getClass());
//                    feval(new String("disp"), rets);
                } catch (Exception e) {
                    //                System.out.println(e.toString());
                }
            }
        }
        System.out.println("About to create dispatch thread to execute command...");
            System.out.println("\t Main thread BEFORE CALLING: "/*+Thread.currentThread().getId()*/+" active count is: "+Thread.activeCount());
        Caller c = new Caller();
        Thread t= new Thread(c);
        t.start();
        
        
        System.out.println("\tThread dispatched... waiting 10 seconds to see if it finished...");
        try {
            System.out.println("\t Main thread: "/*+Thread.currentThread().getId()*/+" active count is: "+Thread.activeCount());
            Thread.sleep(10000);
        } catch (InterruptedException ex) {
            Logger.getLogger(MatlabControl.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        
        System.out.println("K we're done sleeping. What's up?");
        if(c.returnValue==null)System.out.println("testBlockingFeval is returning null");
        else System.out.println("testBlockingFeval is NOT returning null");

        return c.returnValue;
    }

    /********   INTERNAL FUNCTIONS AND CLASSES  *******/
    public void setReturnVal(Object val) {
        synchronized (returnVal) {
            Object oldVal = returnVal;
            returnVal = val;
            oldVal.notifyAll();
        }
    }

    /** This class is used to execute a string in Matlab **/
    protected class MatlabEvalCommand implements Runnable {

        String command;
        boolean useCallback, eval;
        Object[] args;

        public MatlabEvalCommand(String Command, boolean useCallback) {
            command = Command;
            this.useCallback = useCallback;
            eval = true;
            args = null;
        }

        protected Object useMatlabCommandCallback(String command, Object[] args) {
            int numArgs = (args == null) ? 0 : args.length;
            Object newArgs[] = new Object[numArgs + 1];
            newArgs[0] = command;
            for (int i = 0; i < numArgs; i++) {
                newArgs[i + 1] = args[i];
            }
            try {
                return matlab.mtFevalConsoleOutput(new String("matlabControlcb"), newArgs, 0);
            } catch (Exception e) {
                //                System.out.println(e.toString());
                return null;
            }
        }

        public void run() {
            try {
                if (useCallback) {
                    useMatlabCommandCallback(command, args);
                } else if (eval) {
                    matlab.evalConsoleOutput(command);
                } else {
                    matlab.fevalConsoleOutput(command, args);
                }
            } catch (Exception e) {
                System.out.println(e.toString());
            }
        }
    }

    /** This class is used to execute a function in matlab and pass paramseters**/
    protected class MatlabFevalCommand extends MatlabEvalCommand {

        public MatlabFevalCommand(String Command, Object[] Args, boolean useCallback) {
            super(Command, useCallback);
            args = Args;
            eval = false;
        }
    }

    /** This class is used to execute a function in matlab and pass paramseters
     *  and it also return arguments**/
    protected class MatlabBlockingFevalCommand extends MatlabFevalCommand {

        MatlabControl parent;

        public MatlabBlockingFevalCommand(String Command, Object[] Args, boolean useCallback, MatlabControl parent) {
            super(Command, Args, useCallback);
            this.parent = parent;
        }

        public void run() {
            try {
                if (useCallback) {
                    parent.setReturnVal(useMatlabCommandCallback(command, args));
                } else {
                    parent.setReturnVal(matlab.mtFevalConsoleOutput(command, args, 0));
                }
            } catch (Exception e) {
                //                System.out.println(e.toString());
            }
        }
    }
}




