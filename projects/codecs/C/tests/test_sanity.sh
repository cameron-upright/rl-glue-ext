#!/bin/bash

# *  $Revision$
# *  $Date$
# *  $Author$
# *  $HeadURL$


killall rl_glue
killall test_1_agent
killall test_1_environment
killall test_sanity_experiment
sleep 1
rl_glue &
sleep 1
./test_1_agent &
sleep 1
./test_1_environment &
sleep 1
./test_sanity_experiment

test_outcome=$?
exit $test_outcome