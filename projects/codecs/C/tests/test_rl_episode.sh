#!/bin/bash


# *  $Revision$
# *  $Date$
# *  $Author$
# *  $HeadURL$

./pkill.sh rl_glue
./pkill.sh test_1_agent
./pkill.sh test_1_environment
./pkill.sh test_rl_episode_experiment
sleep 1
rl_glue &
sleep 1
./test_1_agent &
sleep 1
./test_1_environment &
sleep 1
./test_rl_episode_experiment 

test_outcome=$?
exit $test_outcome