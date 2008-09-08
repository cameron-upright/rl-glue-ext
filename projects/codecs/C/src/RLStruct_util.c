/*
 *  RLStruct_util.c
 *  
 *
 *  Created by Leah Hackman on 13/06/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */
#include <stdlib.h> /* free, calloc */
#include <string.h> /* memcpy */
#include <rlglue/utils/C/RLStruct_util.h>

/**	
*	Sept 8 2008, Brian Tanner is creating replace function
*	This one takes a src and dst, and puts all the data from the src into the dst
*	Freeing and reallocating if necessary
**/
void replaceRLStruct(const rl_abstract_type_t *src, rl_abstract_type_t *dst){
	if(dst->numInts!=src->numInts){
		if(dst->numInts>0 && dst->intArray!=0){
			free(dst->intArray);
		}
		dst->numInts=src->numInts;
		dst->intArray=(int *)calloc(dst->numInts,sizeof(int));
	}
	if(src->numInts>0){
		memcpy(dst->intArray, src->intArray,dst->numInts);
	}

	if(dst->numDoubles!=src->numDoubles){
		if(dst->numDoubles>0 && dst->doubleArray!=0){
			free(dst->doubleArray);
		}
		dst->numDoubles=src->numDoubles;
		dst->doubleArray=(double *)calloc(dst->numDoubles,sizeof(double));
	}
	if(src->numDoubles>0){
		memcpy(dst->doubleArray, src->doubleArray,dst->numDoubles);
	}

	if(dst->numChars!=src->numChars){
		if(dst->numChars>0 && dst->charArray!=0){
			free(dst->charArray);
		}
		dst->numChars=src->numChars;
		dst->charArray=(char *)calloc(dst->numChars,sizeof(char));
	}
	if(src->numChars>0){
		memcpy(dst->charArray, src->charArray,dst->numChars);
	}
	
}

