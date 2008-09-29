/* 
 * Copyright (C) 2007, Brian Tanner
 * 
http://rl-glue.googlecode.com/

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 * 
 *  $Revision: 151 $
 *  $Date: 2008-09-17 20:09:24 -0600 (Wed, 17 Sep 2008) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: https://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/RLGlue.java $
 * 
 */
package org.rlcommunity.rlglue.codec;

import java.io.IOException;
import org.rlcommunity.rlglue.codec.network.Network;
import org.rlcommunity.rlglue.codec.types.Observation_action;
import org.rlcommunity.rlglue.codec.types.RL_abstract_type;
import org.rlcommunity.rlglue.codec.types.Random_seed_key;
import org.rlcommunity.rlglue.codec.types.Reward_observation_action_terminal;
import org.rlcommunity.rlglue.codec.types.State_key;

/**
 * This is the network connection for an experiment program that will talk to the 
 * C/C++ rl_glue executable over sockets.
 * @since 2.0
 * @author btanner
 */
public class NetGlue implements RLGlueInterface {

    private Network network;
    private String host;
    private int port;

    /**
     * @since 2.0
     * Sets default host and port
     */
    public NetGlue() {
        this(Network.kDefaultHost, Network.kDefaultPort);
    }

    /**
     * @since 2.0
     * Custom host, default port
     */
    public NetGlue(String host) {
        this(host, Network.kDefaultPort);

    }

    /**
     * Specify custom host and port to connect to rl_glue 
     * @since 2.0
     */
    public NetGlue(String host, int port) {
        this.host = host;
        this.port = port;

    }

    public synchronized String RL_init() {
        forceConnection();

        sendEmpty(Network.kRLInit, "RL_init");
        String task_spec = network.getString();
        return task_spec;
    }

    public synchronized Observation_action RL_start() {
        sendEmpty(Network.kRLStart, "RL_start");
        Observation_action obsact = new Observation_action();

        obsact.o = network.getObservation();
        obsact.a = network.getAction();
        return obsact;
    }

    public synchronized Reward_observation_action_terminal RL_step() {
        sendEmpty(Network.kRLStep, "RL_step");

        Reward_observation_action_terminal roat = new Reward_observation_action_terminal();
        roat.terminal = network.getInt();
        roat.r = network.getDouble();
        roat.o = network.getObservation();
        roat.a = network.getAction();

        return roat;
    }

    public synchronized void RL_cleanup() {
        sendEmpty(Network.kRLCleanup, "RL_cleanup");
    }

    public synchronized String RL_agent_message(String message) {
        forceConnection();

        sendString(message, Network.kRLAgentMessage, "RL_agent_message");
        String response = network.getString();
        return response;

    }

    public synchronized String RL_env_message(String message) {
        forceConnection();

        sendString(message, Network.kRLEnvMessage, "RL_env_message");
        String response = network.getString();
        return response;
    }

    public synchronized double RL_return() {
        sendEmpty(Network.kRLReturn, "RL_return");

        double reward = network.getDouble();

        return reward;
    }

    public synchronized int RL_num_steps() {
        sendEmpty(Network.kRLNumSteps, "RL_num_steps");

        int numSteps = network.getInt();

        return numSteps;
    }

    public synchronized int RL_num_episodes() {
        sendEmpty(Network.kRLNumEpisodes, "RL_num_episodes");

        int numEpisodes = network.getInt();

        return numEpisodes;
    }

    public synchronized int RL_episode(int numSteps) {
        sendInt(numSteps, Network.kRLEpisode, "RL_episode");

        int exitStatus = network.getInt();

        return exitStatus;
    }

    public synchronized void RL_set_state(State_key sk) {
        send_abstract_type(sk, Network.kRLSetState, "RL_set_state");
    }

    public synchronized void RL_set_random_seed(Random_seed_key rsk) {
        send_abstract_type(rsk, Network.kRLSetRandomSeed, "RL_set_random_seed");
    }

    public synchronized State_key RL_get_state() {
        sendEmpty(Network.kRLGetState, "RL_get_state");
        State_key key = network.getStateKey();
        return key;
    }

    public synchronized Random_seed_key RL_get_random_seed() {
        sendEmpty(Network.kRLGetRandomSeed, "RL_get_random_seed");
        Random_seed_key key = network.getRandomSeedKey();
        return key;
    }

    /**
     * 
     * 
     * 
     * PRIVATE METHODS BELOW ARE HELPERS
     * 
     * 
     * 
     */
    private void forceConnection() {
        if (network == null) {
            String ImplementationVersion = RLGlueCore.getImplementationVersion();
            String SpecVersion = RLGlueCore.getSpecVersion();

            System.out.println("RL-Glue Java Experiment Codec Version: " + SpecVersion + " (" + ImplementationVersion + ")");
            System.out.println("\tConnecting to " + host + " on port " + port + "...");

            network = new Network();



            // Connect
            network.connect(host, port, Network.kRetryTimeout);
            System.out.println("\tExperiment Codec Connected");
            network.clearSendBuffer();
            network.putInt(Network.kExperimentConnection);
            network.putInt(0);
            network.flipSendBuffer();

            try {
                network.send();
            } catch (IOException ioException) {
                ioException.printStackTrace();
                System.exit(1);
            }
        }
    }

    private synchronized void doStandardRecv(int state) throws IOException {
        network.clearRecvBuffer();

        int recvSize = network.recv(8) - 8;

        int glueState = network.getInt(0);
        int dataSize = network.getInt(Network.kIntSize);
        int remaining = dataSize - recvSize;

        if (remaining < 0) {
            remaining = 0;
        }
        int remainingReceived = network.recv(remaining);

        network.flipRecvBuffer();

        // Discard the header - we should have a more elegant method for doing this.
        network.getInt();
        network.getInt();

        if (glueState != state) {
            System.err.println("Not synched with server. glueState = " + glueState + " but should be " + state);
            System.exit(1);
        }
    }

    private synchronized void doCallWithNoParams(int state) throws IOException {
        network.clearSendBuffer();
        network.putInt(state);
        network.putInt(0);
        network.flipSendBuffer();
        network.send();
    }

    private synchronized void sendString(String theString, int theCode, String callerName) {
        try {
            network.clearSendBuffer();
            network.putInt(theCode);
            network.putInt(Network.sizeOf(theString));
            network.putString(theString);
            network.flipSendBuffer();
            network.send();

            doStandardRecv(theCode);

        } catch (IOException ioException) {
            ioException.printStackTrace();
            System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
            System.exit(1);
        }

    }

    private synchronized void sendEmpty(int theCode, String callerName) {
        try {
            doCallWithNoParams(theCode);
            doStandardRecv(theCode);
        } catch (IOException ioException) {
            ioException.printStackTrace();
            System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
            System.exit(1);
        }

    }

    private synchronized void sendInt(int intToSend, int theCode, String callerName) {
        try {
            network.clearSendBuffer();
            network.putInt(theCode);
            network.putInt(Network.kIntSize);
            network.putInt(intToSend);
            network.flipSendBuffer();
            network.send();

            doStandardRecv(theCode);
        } catch (IOException ioException) {
            ioException.printStackTrace();
            System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
            System.exit(1);
        }

    }

    /**
     * Added by Brian Tanner to simplify the code in here.
     * @param theObject
     * @param theCode
     * @param callerName
     */
    private synchronized void send_abstract_type(RL_abstract_type theObject, int theCode, String callerName) {
        try {
            network.clearSendBuffer();
            network.putInt(theCode);
            network.putInt(Network.sizeOf(theObject));
            network.putAbstractType(theObject);
            network.flipSendBuffer();
            network.send();

            doStandardRecv(theCode);
        } catch (IOException ioException) {
            ioException.printStackTrace();
            System.exit(1);
        } catch (NullPointerException nullException) {
            System.err.println("You must call RL_init before calling " + callerName);
            nullException.printStackTrace();
            System.exit(1);
        }

    }
}
