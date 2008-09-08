#ifndef RLStruct_Util_H
#define RLStruct_Util_H

#ifdef __cplusplus
extern "C" {
#endif

#include <rlglue/RL_common.h>


/**	
*	Sept 8 2008, Brian Tanner is creating replace function
*	This one takes a src and dst, and puts all the data from the src into the dst
*	Freeing and reallocating if necessary
**/
void replaceRLStruct(const rl_abstract_type_t *src, rl_abstract_type_t *dst);

#ifdef __cplusplus
}
#endif
#endif
