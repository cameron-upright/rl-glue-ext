TESTS = test_sanity.sh test_1.sh test_message.sh test_rl_episode.sh

check_PROGRAMS = test_sanity_experiment test_1_agent test_1_environment test_1_experiment test_message_agent test_message_environment test_message_experiment test_rl_episode_experiment

AM_CFLAGS=-g
test_sanity_experiment_SOURCES = test_sanity_experiment.c useful_functions.c useful_functions.h
test_sanity_experiment_LDADD = ../src/librlexperiment.la

test_1_agent_SOURCES = test_1_agent.c useful_functions.c useful_functions.h
test_1_agent_LDADD = ../src/librlagent.la

test_1_environment_SOURCES = test_1_environment.c useful_functions.c useful_functions.h
test_1_environment_LDADD = ../src/librlenvironment.la

test_1_experiment_SOURCES = test_1_experiment.c useful_functions.c useful_functions.h
test_1_experiment_LDADD = ../src/librlexperiment.la


test_message_agent_SOURCES = test_message_agent.c useful_functions.c useful_functions.h
test_message_agent_LDADD = ../src/librlagent.la

test_message_environment_SOURCES = test_message_environment.c useful_functions.c useful_functions.h
test_message_environment_LDADD = ../src/librlenvironment.la

test_message_experiment_SOURCES = test_message_experiment.c useful_functions.c useful_functions.h
test_message_experiment_LDADD = ../src/librlexperiment.la

test_rl_episode_experiment_SOURCES = test_rl_episode_experiment.c useful_functions.c useful_functions.h
test_rl_episode_experiment_LDADD = ../src/librlexperiment.la


INCLUDES = -I$(top_srcdir)/src
# LDADD = ../src/librlglue.la
# 
# test1_SOURCES = test1_environment.c	test1_agent.c experiment.c useful_functions.c useful_functions.h
# test1_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99
# 
# message_test_SOURCES = message_test_agent.c	message_test_environment.c message_test_experiment.c useful_functions.c useful_functions.h
# message_test_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99
# 
# rl_episode_test_experiment_SOURCES = test1_environment.c	test1_agent.c rl_episode_test_experiment.c useful_functions.c useful_functions.h
# rl_episode_test_experiment_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99
# 
# noinst_PROGRAMS = $(TESTS)
