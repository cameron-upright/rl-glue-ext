/*
 *  RLStruct_util.c
 *  
 *
 *  Created by Leah Hackman on 13/06/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <rlglue/utils/C/RLStruct_util.h>

rl_abstract_type_t copyRLStruct(rl_abstract_type_t newStruct)
{
	rl_abstract_type_t oldStruct={0};

//Copy the contents of an old RL_abstract_type to the new RL_abstract_type
	unsigned int i =0;
	oldStruct.numInts = newStruct.numInts;
	oldStruct.numDoubles = newStruct.numDoubles;
	for(i=0; i<newStruct.numInts; i++)
	oldStruct.intArray[i] = newStruct.intArray[i];
	for(i=0;i<newStruct.numDoubles;i++)
	oldStruct.doubleArray[i] = newStruct.doubleArray[i];
	
	return oldStruct;
}
