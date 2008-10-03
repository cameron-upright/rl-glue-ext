#!/bin/sh

TEST="test-speed"
source ../test.sh
run_test "../test-1/RL_agent" \
         "./RL_environment" \
         "./RL_experiment"

