#!/bin/bash

# *  $Revision$
# *  $Date$
# *  $Author$
# *  $HeadURL$

./pkill.sh rl_glue
./pkill.sh test_empty_agent
./pkill.sh test_empty_environment
./pkill.sh test_empty_experiment
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