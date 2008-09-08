#ifndef RLStruct_Util_H
#define RLStruct_Util_H

#ifdef __cplusplus
extern "C" {
#endif

#include <rlglue/RL_common.h>
/* As of Sept 8/2008 I think this function might be very broken */
rl_abstract_type_t copyRLStruct(rl_abstract_type_t newStruct);

#ifdef __cplusplus
}
#endif
#endif
