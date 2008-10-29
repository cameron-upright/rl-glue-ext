/* 
 * Copyright (C) 2007, Brian Tanner
 * 
http://rl-glue-ext.googlecode.com/

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
 *  $Revision$
 *  $Date$
 *  $Author$
 *  $HeadURL$
 * 
 */
package org.rlcommunity.rlglue.codec.network;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.RL_abstract_type;
import org.rlcommunity.rlglue.codec.types.Random_seed_key;
import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;
import org.rlcommunity.rlglue.codec.types.State_key;

/**
 * This class does the heavy lifting of sendig and receiving data over the 
 * network. It is used by both the Java and Matlab codecs.
 * @author btanner
 */
public class Network {

    public static final int kExperimentConnection = 1;
    public static final int kAgentConnection = 2;
    public static final int kEnvironmentConnection = 3;
    public static final int kAgentInit = 4;
    public static final int kAgentStart = 5;
    public static final int kAgentStep = 6;
    public static final int kAgentEnd = 7;
    public static final int kAgentCleanup = 8;
    public static final int kAgentFreeze = 9;
    public static final int kAgentMessage = 10;
    public static final int kEnvInit = 11;
    public static final int kEnvStart = 12;
    public static final int kEnvStep = 13;
    public static final int kEnvCleanup = 14;
    public static final int kEnvSetState = 15;
    public static final int kEnvSetRandomSeed = 16;
    public static final int kEnvGetState = 17;
    public static final int kEnvGetRandomSeed = 18;
    public static final int kEnvMessage = 19;
    public static final int kRLInit = 20;
    public static final int kRLStart = 21;
    public static final int kRLStep = 22;
    public static final int kRLCleanup = 23;
    public static final int kRLReturn = 24;
    public static final int kRLNumSteps = 25;
    public static final int kRLNumEpisodes = 26;
    public static final int kRLEpisode = 27;
    public static final int kRLSetState = 28;
    public static final int kRLSetRandomSeed = 29;
    public static final int kRLGetState = 30;
    public static final int kRLGetRandomSeed = 31;
    //RLFreeze is deprecated
    public static final int kRLFreeze = 32;
    public static final int kRLAgentMessage = 33;
    public static final int kRLEnvMessage = 34;
    public static final int kRLTerm = 35;
    public static final String kDefaultHost = "127.0.0.1";
    public static final int kDefaultPort = 4096;
    public static final int kRetryTimeout = 2;
    protected static final int kByteBufferDefaultSize = 4096;
    public static final int kIntSize = 4;
    protected static final int kDoubleSize = 8;
    protected static final int kCharSize = 1;
    protected SocketChannel socketChannel;
    protected ByteBuffer recvBuffer;
    protected ByteBuffer sendBuffer;
    private boolean debug = false;

    public Network() {
        recvBuffer = ByteBuffer.allocateDirect(kByteBufferDefaultSize);
        sendBuffer = ByteBuffer.allocateDirect(kByteBufferDefaultSize);
    }

    public void connect(String host, int port, int retryTimeout) {
        boolean didConnect = false;

        while (!didConnect) {
            try {
                InetSocketAddress address = new InetSocketAddress(host, port);
                socketChannel = SocketChannel.open();
                socketChannel.configureBlocking(true);
                socketChannel.connect(address);
                didConnect = true;
            } catch (IOException ioException) {
                try {
                    Thread.sleep(retryTimeout);
                } catch (InterruptedException ieException) {
                }
            }
        }
    }

    public void close() throws IOException {
        socketChannel.close();
    }

    public int send() throws IOException {
        return socketChannel.write(sendBuffer);
    }

    public int recv(int size) throws IOException {
        this.ensureRecvCapacityRemains(size);

        int recvSize = 0;
        while (recvSize < size) {
            recvSize += socketChannel.read(recvBuffer);
        }
        return recvSize;
    }

    public void clearSendBuffer() {
        sendBuffer.clear();
    }

    public void clearRecvBuffer() {
        recvBuffer.clear();
    }

    public void flipSendBuffer() {
        sendBuffer.flip();
    }

    public void flipRecvBuffer() {
        recvBuffer.flip();
    }

    public int[] getInts(int howMany) {
        int currentPosition = recvBuffer.position();
        int[] returnArray = new int[howMany];
        recvBuffer.asIntBuffer().get(returnArray);
        recvBuffer.position(currentPosition + howMany * 4);
        return returnArray;
    }

    public double[] getDoubles(int howMany) {
        int currentPosition = recvBuffer.position();
        double[] returnArray = new double[howMany];
        recvBuffer.asDoubleBuffer().get(returnArray);
        recvBuffer.position(currentPosition + howMany * 8);
        return returnArray;
    }

    public int getInt() {
        return recvBuffer.getInt();
    }

    public int getInt(int index) {
        return recvBuffer.getInt(index);
    }

    public double getDouble() {
        return recvBuffer.getDouble();
    }

    /**
     * @since 2.0
     * Pull a char off the buffer.  Slightly tricky because chars are unicode
     * (more than a byte) in Java, but only 1 byte in our network protocol.
     * <p>Thanks: http://bytes.com/forum/thread17160.html
     * @return ascii character encoded by the next byte.
     */
    public char getChar() {
        byte b = recvBuffer.get();
        return (char) (b & 0xFF);
    }

    /**
     * Used for getting task spec and env/agent messages. UNLIKE the
     * charArrays in observations/actions/etc, these strings are null
     * terminated?
     * @return Return a String from the network buffer
     */
    public String getString() {
        String returnString = "";
        int recvLength = this.getInt();
        byte[] recvByteBuffer = new byte[recvLength];
        recvBuffer.get(recvByteBuffer);
        try {

            returnString = new String(recvByteBuffer, "UTF-8");
        } catch (UnsupportedEncodingException ex) {
            System.err.println("Exception reading String from buffer: " + ex);
            ex.printStackTrace();
        }
        return returnString;

    }

    public Observation getObservation() {
        Observation returnVal=new Observation();
        fillAbstractType(returnVal);
        return returnVal;
    }

    public Action getAction() {
        Action returnVal=new Action();
        fillAbstractType(returnVal);
        return returnVal;
    }

    public State_key getStateKey() {
        State_key returnVal=new State_key();
        fillAbstractType(returnVal);
        return returnVal;
    }

    public Random_seed_key getRandomSeedKey() {
        Random_seed_key returnVal=new Random_seed_key();
        fillAbstractType(returnVal);
        return returnVal;
    }

    /*
     * 
     * Hmm, this method might actually make it quite expensive to make abstract types because
     * we need to read them once, then make a copy immediately when we change them
     * into specialized supertypes...
     * @deprecated
     */ 
    private final RL_abstract_type getAbstractType() {
        final int numInts = this.getInt();
        final int numDoubles = this.getInt();
        final int numChars = this.getInt();

        RL_abstract_type key = new RL_abstract_type(numInts, numDoubles, numChars);

        key.intArray = getInts(numInts);
        key.doubleArray = getDoubles(numDoubles);

        for (int i = 0; i < numChars; ++i) {
            key.charArray[i] = this.getChar();
        }

        return key;
    }
    
    private final void fillAbstractType(RL_abstract_type toFill) {
        final int numInts = getInt();
        final int numDoubles = getInt();
        final int numChars = getInt();

        toFill.intArray = getInts(numInts);
        toFill.doubleArray = getDoubles(numDoubles);
        
        toFill.charArray=new char[numChars];
        for (int i = 0; i < numChars; ++i) {
            toFill.charArray[i] = this.getChar();
        }
    }

    /**
     * Experimental
     * @param values
     */
    public void putInts(int[] values) {
        if (values == null) {
            return;
        }
        this.ensureSendCapacityRemains(Network.kIntSize * values.length);
        int currentPosition = sendBuffer.position();
        sendBuffer.asIntBuffer().put(values);
        //Using the intBuffer view doesn't increment the position in the underlying
        //buffer so we have to push it along
        sendBuffer.position(currentPosition + values.length * 4);

    }

    /**
     * Experimental
     * @param values
     */
    public void putDoubles(double[] values) {
        if (values == null) {
            return;
        }
        this.ensureSendCapacityRemains(Network.kDoubleSize * values.length);
        int currentPosition = sendBuffer.position();
        sendBuffer.asDoubleBuffer().put(values);
        sendBuffer.position(currentPosition + values.length * 8);

    }

    public void putInt(int value) {
        this.ensureSendCapacityRemains(Network.kIntSize);
        this.sendBuffer.putInt(value);
    }

    public void putDouble(double value) {
        this.ensureSendCapacityRemains(Network.kDoubleSize);
        this.sendBuffer.putDouble(value);
    }

    /**
     * Brian Tanner adding this for RL-Glue 3.x compatibility
     * Converts unicode (> 1 byte) char to 1 byte network char protocol
     * <p>Thanks http://bytes.com/forum/thread17160.html
     * @since 2.0
     * @param c character to put into the buffer as byte
     */
    public void putChar(char c) {
        this.ensureSendCapacityRemains(Network.kCharSize);
        this.sendBuffer.put((byte) (c & 0xFF));
    }

    public void putString(String message) throws UnsupportedEncodingException {
        // We don't want to have to deal null...
        if (message == null) {
            message = "";
        }
        this.ensureSendCapacityRemains(Network.kIntSize + message.length());
        this.putInt(message.length());

        if (message.length() > 0) {
            sendBuffer.put(message.getBytes("UTF-8"));
        }
    }

    public final void putAbstractType(RL_abstract_type theObject) {
        this.ensureSendCapacityRemains(Network.sizeOf(theObject));

        int numInts = 0;
        int numDoubles = 0;
        int numChars = 0;

        if (theObject != null) {
            if (theObject.intArray != null) {
                numInts = theObject.intArray.length;
            }
            if (theObject.doubleArray != null) {
                numDoubles = theObject.doubleArray.length;
            }
            if (theObject.charArray != null) {
                numChars = theObject.charArray.length;
            }
        }


        this.putInt(numInts);
        this.putInt(numDoubles);
        this.putInt(numChars);
        putInts(theObject.intArray);
        putDoubles(theObject.doubleArray);

        //Not implementing a putChars because each char is currently
        //converted manually from a byte
        for (int i = 0; i < numChars; ++i) {
            this.putChar(theObject.charArray[i]);
        }
    }

    public void putObservation(Observation obs) {
        putAbstractType(obs);
    }

    public void putAction(Action action) {
        putAbstractType(action);
    }

    public void putStateKey(State_key key) {
        putAbstractType(key);
    }

    public void putRandomSeedKey(Random_seed_key key) {
        putAbstractType(key);
    }

    public void putRewardObservation(Reward_observation_terminal rewardObservation) {
        this.ensureSendCapacityRemains(Network.sizeOf(rewardObservation));

        this.putInt(rewardObservation.terminal);
        this.putDouble(rewardObservation.r);
        this.putObservation(rewardObservation.o);
    }

    protected void ensureSendCapacityRemains(int capacity) {
        if (sendBuffer.capacity() - sendBuffer.position() < capacity) {
            sendBuffer = Network.cloneWithCapacity(sendBuffer, sendBuffer.capacity() + capacity);
        }
    }

    protected void ensureRecvCapacityRemains(int capacity) {
        if (recvBuffer.capacity() - recvBuffer.position() < capacity) {
            recvBuffer = Network.cloneWithCapacity(recvBuffer, recvBuffer.capacity() + capacity);
        }
    }

    protected static ByteBuffer cloneWithCapacity(ByteBuffer original, int capacity) {
        ByteBuffer clone = ByteBuffer.allocateDirect(capacity);
        original.flip();
        clone.put(original);
        clone.position(original.position());
        return clone;
    }

    public static int sizeOf(int value) {
        return Network.kIntSize;
    }

    public static int sizeOf(double value) {
        return Network.kDoubleSize;
    }

    public static int sizeOf(String string) {
        int size = Network.kIntSize;
        if (string != null) {
            size += string.length();
        }
        return size;
    }

    public static int sizeOf(RL_abstract_type theObject) {
        int size = Network.kIntSize * 3;
        int intSize = 0;
        int doubleSize = 0;
        int charSize = 0;

        if (theObject != null) {
            if (theObject.intArray != null) {
                intSize = Network.kIntSize * theObject.intArray.length;
            }
            if (theObject.doubleArray != null) {
                doubleSize = Network.kDoubleSize * theObject.doubleArray.length;
            }
            if (theObject.charArray != null) {
                charSize = Network.kCharSize * theObject.charArray.length;
            }
        }
        return size + intSize + doubleSize + charSize;
    }

    public static int sizeOf(Reward_observation_terminal rewardObservation) {
        return Network.kIntSize + // terminal
                Network.kDoubleSize + // reward
                Network.sizeOf(rewardObservation.o);
    }
}

