#!/bin/bash

# *  $Revision: 113 $
# *  $Date: 2008-09-12 21:40:04 -0600 (Fri, 12 Sep 2008) $
# *  $Author: brian@tannerpages.com $
# *  $HeadURL: https://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/tests/test_1.sh $

killall rl_glue
killall test_1_agent
killall test_seeds_environment
killall test_seeds_experiment
sleep 1
rl_glue &
sleep 1
./test_1_agent &
sleep 1
./test_seeds_environment &
sleep 1
./test_seeds_experiment

test_outcome=$?
exit $test_outcome