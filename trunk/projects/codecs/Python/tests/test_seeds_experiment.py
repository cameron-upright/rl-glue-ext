# 
# Copyright (C) 2008, Brian Tanner
# 
#http://rl-glue.googlecode.com/
#
# Licensed under the Apache License, Version 2.0 (the "License")
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

import sys

import rlglue.RLGlue as RLGlue
from glue_test import glue_test
from rlglue.types import State_key
from rlglue.types import Random_seed_key

# from glue_test import TestUtility
tester =glue_test("test_seeds")



the_state_key=State_key()
empty_state_key=State_key()
the_random_seed=Random_seed_key()
empty_random_seed=Random_seed_key()

# TestUtility.clean_abstract_type(the_state_key)
# TestUtility.clean_abstract_type(the_random_seed)
# TestUtility.clean_abstract_type(empty_state_key)
# TestUtility.clean_abstract_type(empty_random_seed)

# TestUtility.set_k_ints_in_abstract_type(the_state_key,3)
# TestUtility.set_k_doubles_in_abstract_type(the_state_key,7)
# TestUtility.set_k_chars_in_abstract_type(the_state_key,2)
the_state_key.intArray=[0, 1, 2]
the_state_key.doubleArray=[0.0/7.00, 1.0/7.00,2.0/7.00,3.0/7.00,4.0/7.00,5.0/7.00,6.0/7.00]
the_state_key.charArray=['a','b']

the_random_seed.intArray=[0]
the_random_seed.doubleArray=[0.0/2.00, 1.0/2.00]
the_random_seed.charArray=['a','b','c','d']

#/*	compare_abstract_types */

RLGlue.RL_init()

RLGlue.RL_set_state(the_state_key)
returned_state_key=RLGlue.RL_get_state()
tester.check_fail(not the_state_key.sameAs(returned_state_key))


RLGlue.RL_set_random_seed(the_random_seed)
returned_random_seed_key=RLGlue.RL_get_random_seed()
tester.check_fail(not the_random_seed.sameAs(returned_random_seed_key))


the_state_key.intArray=[]
the_state_key.doubleArray=[]
the_state_key.charArray=[]


the_random_seed.intArray=[]
the_random_seed.doubleArray=[]
the_random_seed.charArray=[]


RLGlue.RL_set_state(the_state_key)
returned_state_key=RLGlue.RL_get_state()
tester.check_fail(not the_state_key.sameAs(returned_state_key))

RLGlue.RL_set_random_seed(the_random_seed)
returned_random_seed_key=RLGlue.RL_get_random_seed()
tester.check_fail(not the_random_seed.sameAs(returned_random_seed_key))

#/* Make sure if we send an empty we get back an empty */
RLGlue.RL_set_state(empty_state_key)
returned_state_key=RLGlue.RL_get_state()
tester.check_fail(not empty_state_key.sameAs(returned_state_key))

RLGlue.RL_set_random_seed(empty_random_seed)
returned_random_seed_key=RLGlue.RL_get_random_seed()
tester.check_fail(not empty_random_seed.sameAs(returned_random_seed_key))

		

print tester.get_summary()
exit(tester.getFailCount())

