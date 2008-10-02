#!/bin/sh

TEST="test-empty"
source ../test.sh
run_test "./RL_agent" \
         "./RL_environment" \
         "./RL_experiment"

