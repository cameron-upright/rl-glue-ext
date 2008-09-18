#!/bin/bash

# *  $Revision$
# *  $Date$
# *  $Author$
# *  $HeadURL$

killall rl_glue
killall test_1_agent
killall test_speed_environment
killall test_speed_experiment
sleep 1
rl_glue &
sleep 1
./test_1_agent &
sleep 1
./test_speed_environment &
sleep 1
./test_speed_experiment

test_outcome=$?
exit $test_outcome