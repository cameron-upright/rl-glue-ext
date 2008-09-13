TESTS = test_sanity.sh test_1.sh test_message.sh test_rl_episode.sh test_seeds.sh

check_PROGRAMS = test_sanity_experiment test_1_agent test_1_environment test_1_experiment test_message_agent test_message_environment test_message_experiment  test_rl_episode_experiment test_seeds_experiment test_seeds_environment

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

test_seeds_experiment_SOURCES = test_seeds_experiment.c useful_functions.c useful_functions.h
test_seeds_experiment_LDADD = ../src/librlexperiment.la

test_seeds_environment_SOURCES = test_seeds_environment.c useful_functions.c useful_functions.h
test_seeds_environment_LDADD = ../src/librlenvironment.la

INCLUDES = -I$(top_srcdir)/src

