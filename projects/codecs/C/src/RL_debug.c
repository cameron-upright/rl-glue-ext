#include <assert.h>
#include <rlglue/RL_common.h>
#include <stdio.h>


int __rlglue_check_abstract_type(const rl_abstract_type_t *theStruct){
	if(theStruct->numInts>1000)return 1;
	if(theStruct->numDoubles>1000)return 2;
	if(theStruct->numChars>1000)return 3;

	if(theStruct->numInts>0 && theStruct->intArray==0)return 4;
	if(theStruct->numDoubles>0 && theStruct->doubleArray==0)return 5;
	if(theStruct->numChars>0 && theStruct->charArray==0)return 6;

	if(theStruct->numInts==0 && theStruct->intArray!=0)return 7;
	if(theStruct->numDoubles==0 && theStruct->doubleArray!=0)return 8;
	if(theStruct->numChars==0 && theStruct->charArray!=0)return 9;
	
	return 0;
}

/* Take ths out later */
void __rlglue_print_abstract_type(const rl_abstract_type_t *theStruct){
	int i;
	__RL_CHECK_STRUCT(theStruct)
	
	printf("Printing Abstract Type\n-----------------\n");
	printf("\t Ints: %d \t Doubles: %d\t Chars: %d\n",theStruct->numInts, theStruct->numDoubles, theStruct->numChars);
	
	if(theStruct->numInts<100){
		printf("Ints: ");
		for(i=0;i<theStruct->numInts;i++)
			printf("\t%d",theStruct->intArray[i]);
	}
	printf("\n");
	if(theStruct->numDoubles<100){
		printf("Doubles: ");
		for(i=0;i<theStruct->numDoubles;i++)
			printf("\t%f",theStruct->doubleArray[i]);
	}
	printf("\n");
	if(theStruct->numChars<100){
		printf("Chars: ");
		for(i=0;i<theStruct->numChars;i++)
			printf("\t%c",theStruct->charArray[i]);
	}
	printf("\n");
}
