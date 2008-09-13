#!/bin/bash
killall rl_glue
killall test_message_agent
killall test_message_environment
killall test_message_experiment
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