#!/bin/sh

TEST="test-message"
source ../test.sh
run_test "./RL_agent" \
         "./RL_environment" \
         "./RL_experiment"

