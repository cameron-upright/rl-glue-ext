# 
# Copyright (C) 2008, Brian Tanner
# 
#http://rl-glue-ext.googlecode.com/
#
# Licensed under the Apache License, Version 2.0 (the "License");
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
# Last modifed 22-1-2009 by Jose Antonio Martin H.
# Improving the test processTaskSpec 

import sys

from rlglue.utils import TaskSpecVRLGLUE3 
from glue_test import glue_test
tester =glue_test("test_taskspec")

def processTaskSpec(ts):
# you can cut the taskspec by the main words with new line
#ts= """VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1 OBSERVATIONS INTS (3 0 1) DOUBLES (2 -1.2 0.5) (-.07 .07) CHARCOUNT 1024
#     ACTIONS INTS (2 0 4) CHARCOUNT 1024 REWARDS (-5.0 UNSPEC) EXTRA some other stuff goes here"""
        print
        print
        print "======================================================================================================="
        print ts
        print 
        print
        TaskSpec = TaskSpecVRLGLUE3.TaskSpecParser(ts)
        if TaskSpec.valid:                
                print "======================================================================================================="
                print "Version: ["+TaskSpec.getVersion()+"]"
                print "ProblemType: ["+TaskSpec.getProblemType()+"]"
                print "DiscountFactor: ["+TaskSpec.getDiscountFactor()+"]"
                print "======================================================================================================="
                print "\t \t \t \t Observations"
                print "======================================================================================================="
                print "Observations: ["+TaskSpec.getObservations()+"]"
                print "Integers:",TaskSpec.getIntObservations()
                print "Doubles: ",TaskSpec.getDoubleObservations()
                print "Chars:   ",TaskSpec.getCharCountObservations()
                print "======================================================================================================="
                print "\t \t \t \t Actions"
                print "======================================================================================================"
                print "Actions: ["+TaskSpec.getActions()+"]"
                print "Integers:",TaskSpec.getIntActions()
                print "Doubles: ",TaskSpec.getDoubleActions()
                print "Chars:   ",TaskSpec.getCharCountActions()
                print "======================================================================================================="        
                print "Reward :["+TaskSpec.getReward()+"]"
                print "Reward Range:",TaskSpec.getRewardRange()
                print "Extra: ["+TaskSpec.getExtra()+"]"
                print "remeber that by using len() you get the cardinality of lists!"
                print "Thus:"
                print "len(Doubles) ==> ",len(TaskSpec.getDoubleObservations())," Double Observations"



f=open('sample_task_specs.txt', 'r')

for ts in f:
                processTaskSpec(ts)

f.close()

# task_spec=RLGlue.RL_init();
# 
# RLGlue.RL_start();
# 
# roat=RLGlue.RL_step();
# 
# 
# 
# tester.check_fail(len(roat.o.intArray)!=1);
# tester.check_fail(len(roat.o. doubleArray)!=0);
# tester.check_fail(len(roat.o. charArray)!=0);
# tester.check_fail(roat.o.intArray[0]!=0);
# tester.check_fail("one|1.|one"!=RLGlue.RL_env_message("one"));
# tester.check_fail("one|1.|one"!=RLGlue.RL_agent_message("one"));
# 
# tester.check_fail(roat.terminal!=0);
# 
# 
# roat=RLGlue.RL_step();
# 
# tester.check_fail("two|2.2.|two"!=RLGlue.RL_env_message("two"));
# tester.check_fail("two|2.2.|two"!=RLGlue.RL_agent_message("two"));
# tester.check_fail(roat.terminal!=0);
# tester.check_fail(len(roat.o.intArray)!=1);
# tester.check_fail(len(roat.o. doubleArray)!=0);
# tester.check_fail(len(roat.o. charArray)!=0);
# tester.check_fail(roat.o.intArray[0]!=1);
# 
# roat=RLGlue.RL_step();
# 
# tester.check_fail("three||three"!=RLGlue.RL_env_message("three"));
# tester.check_fail("three||three"!=RLGlue.RL_agent_message("three"));
# tester.check_fail(roat.terminal!=0);
# tester.check_fail(len(roat.o.intArray)!=1);
# tester.check_fail(len(roat.o. doubleArray)!=0);
# tester.check_fail(len(roat.o. charArray)!=0); 
# tester.check_fail(roat.o.intArray[0]!=2);
# 
# roat=RLGlue.RL_step();
# tester.check_fail("four|4.|four"!=RLGlue.RL_env_message("four"));
# tester.check_fail("four|4.|four"!=RLGlue.RL_agent_message("four"));
# tester.check_fail(roat.terminal!=0);
# tester.check_fail(len(roat.o.intArray)!=1);
# tester.check_fail(len(roat.o. doubleArray)!=0);
# tester.check_fail(len(roat.o. charArray)!=0);
# tester.check_fail(roat.o.intArray[0]!=3);
# 
# 
# roat=RLGlue.RL_step();
# tester.check_fail("five|5.5.|five"!=RLGlue.RL_env_message("five"));
# tester.check_fail("five|4.|five"!=RLGlue.RL_agent_message("five"));
# tester.check_fail(roat.terminal==0);

print tester.get_summary()
exit(tester.getFailCount())


