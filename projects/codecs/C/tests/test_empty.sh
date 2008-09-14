#!/bin/bash

# *  $Revision$
# *  $Date$
# *  $Author$
# *  $HeadURL$

killall rl_glue
killall test_empty_agent
killall test_empty_environment
killall test_empty_experiment
sleep 1
rl_glue &
sleep 1
./test_empty_agent &
sleep 1
./test_empty_environment &
sleep 1
./test_empty_experiment

test_outcome=$?
exit $test_outcome