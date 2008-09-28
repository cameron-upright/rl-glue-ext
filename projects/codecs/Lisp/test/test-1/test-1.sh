#!/bin/sh

TEST="test-1"
source ../test.sh
run_test "./RL_agent" \
         "./RL_environment" \
         "./RL_experiment"

