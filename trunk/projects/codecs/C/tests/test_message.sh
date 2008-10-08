#!/bin/bash

# *  $Revision$
# *  $Date$
# *  $Author$
# *  $HeadURL$

./pkill.sh rl_glue
./pkill.sh test_message_agent
./pkill.sh test_message_environment
./pkill.sh test_message_experiment
sleep 1
rl_glue &
sleep 1
./test_message_agent &
sleep 1
./test_message_environment &
sleep 1
./test_message_experiment

test_outcome=$?
exit $test_outcome