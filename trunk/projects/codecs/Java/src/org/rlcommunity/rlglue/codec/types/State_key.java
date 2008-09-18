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
*  $Revision$
*  $Date$
*  $Author$
*  $HeadURL$
* 
*/


package org.rlcommunity.rlglue.codec.types;

public class State_key extends RL_abstract_type
{
	public State_key() 
	{
		this(0,0,0);
	}

	/**
         * For backwards compatibility wiht RL-Glue 2.x
         * @param numInts
         * @param numDoubles
         * @param numChars
         */
        public State_key(int numInts, int numDoubles)
	{
		this(numInts,numDoubles,0);
	}

        public State_key(int numInts, int numDoubles, int numChars)
	{
		super(numInts,numDoubles,numChars);
	}       
	public State_key(RL_abstract_type src)
	{
		super(src);
	}
        

        public State_key duplicate(){
            return new State_key(this);
        }

}
