/* 
* Copyright (C) 2007, Brian Tanner

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include "RandomAgent.h"

action_t action;

int freeze = 0;
task_spec_struct tss;					/*declare task_spec_struct*/

void randomify(action_t action);

void agent_init(const task_specification_t task_spec)
{
  srand(0);/*seed the randomness*/
	srand48(0);
  
  assert (task_spec != 0);
  parse_task_spec(task_spec, &tss);		/*Parsing task_specification*/	

/*allocating memory for one Action*/
  action.numInts     =  1;//tss.num_discrete_action_dims;
  action.intArray    = (int*)malloc(sizeof(int)*action.numInts);
  action.numDoubles  = 0;//tss.num_continuous_action_dims;
  action.doubleArray = 0;

}

action_t agent_start(observation_t o) {
	randomify(action);
	/*ask(o,action);*/
	return action;
}

action_t agent_step(reward_t reward, observation_t o) {
	printf("The observation char array is: %s\n",o.charArray);
	randomify(action);
	return action;
}

void agent_end(reward_t reward) {
}

void agent_cleanup() {
}

void agent_freeze() {
}

message_t agent_message(const message_t message) {
	return NULL;
}


double getValueForState(observation_t theObservation) {
	return 0;
}
	

void randomify(action_t action){
	int i;
	for(i=0;i<tss.num_discrete_action_dims;i++){
		action.intArray[i] = rand()%((int)tss.action_maxs[i]+1-(int)tss.action_mins[i]) + (int)tss.action_mins[i];
	}
	for(i=0;i<tss.num_continuous_action_dims;i++){
		action.doubleArray[i] = drand48()*(tss.action_maxs[i]-tss.action_mins[i]) + tss.action_mins[i];
	}
}
