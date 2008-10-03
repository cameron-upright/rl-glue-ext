#!/bin/sh

TEST="test-sanity"
source ../test.sh
run_test "../test-1/RL_agent" \
         "../test-1/RL_environment" \
         "./RL_experiment"

