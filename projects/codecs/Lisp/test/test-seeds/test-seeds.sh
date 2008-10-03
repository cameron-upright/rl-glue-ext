#!/bin/sh

TEST="test-seeds"
source ../test.sh
run_test "../test-1/RL_agent" \
         "./RL_environment" \
         "./RL_experiment"

