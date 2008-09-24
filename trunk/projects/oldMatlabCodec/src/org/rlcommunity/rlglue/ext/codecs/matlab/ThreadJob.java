/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.rlcommunity.rlglue.ext.codecs.matlab;

public class ThreadJob implements Runnable {

        private int id = 0;

        public ThreadJob(int num){

                id = num;

        }

        public void run(){

                System.out.println("Starting job " + id + "."); 

                try {

                        Thread.sleep(15000);

                } catch (InterruptedException e) {

                        e.printStackTrace();

                }

                System.out.println("Finished job " + id + "."); 

        }
        public static void beABitch(){
             System.out.println("beABitch is sleeping");
             
                try {

                        Thread.sleep(15000);

             System.out.println("beABitch snored and woke up ... back to sleeping");

                        Thread.sleep(15000);

                } catch (InterruptedException e) {

                        e.printStackTrace();

                }

                System.out.println("beABitch Finished "); 
            
        }
        
        public static void main(){

                for(int i = 0; i < 20; i++){

                        Thread t = new Thread(new ThreadJob(i));
                        
                        t.start();
                        
                        System.out.println("In main() -- going to sleep for 3 seconds");
                        

                }

        }

}