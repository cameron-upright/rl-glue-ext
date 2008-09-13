/* 
* Copyright (C) 2007, Mark Lee

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
#include <stdlib.h>
#include <stdio.h>

#include <string.h>
#include <math.h>
#include <float.h>


#ifndef C_TASKSPEC_PARSER_H
#define C_TASKSPEC_PARSER_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef INFINITY
#define INFINITY (1.0f/0.0f)
#endif
#define GLUE_NAN (sqrt(-1.0))

typedef struct 
{
        float version;			
        char episodic;			
        int obs_dim;			
        int num_discrete_obs_dims;
        int num_continuous_obs_dims;
        char *obs_types;	    
        double *obs_mins;           
        double *obs_maxs;			
        int action_dim;			
        int num_discrete_action_dims;
        int num_continuous_action_dims;
        char *action_types;		
        double *action_mins;		
        double *action_maxs;
		double reward_min;
		double reward_max;
} task_spec_struct;


void parse_task_spec(const char* ts, task_spec_struct* ps);

#ifdef __cplusplus
}
#endif

#endif /*C_TASKSPEC_PARSER_H*/
