CODEC_RELEASE=1:0:0
CODEC_VERSION=1:0:0

lib_LTLIBRARIES = libtaskspecutils.la librlagent.la librlenvironment.la librlexperiment.la
 
libtaskspecutils_la_SOURCES = TaskSpec_Parser.c rlglue/utils/C/TaskSpec_Parser.h
libtaskspecutils_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE) -no-missing
libtaskspecutils_la_includedir=$(includedir)/rlglue/utils/C
libtaskspecutils_la_include_HEADERS = rlglue/utils/C/TaskSpec_Parser.h
libtaskspecutils_la_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99 -g

librlagent_la_SOURCES = RL_client_agent.c RL_debug.c
librlagent_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
librlagent_la_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99 -g

librlenvironment_la_SOURCES = RL_client_environment.c RL_debug.c
librlenvironment_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
librlenvironment_la_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99 -g

librlexperiment_la_SOURCES = RL_client_experiment.c RL_debug.c
librlexperiment_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
librlexperiment_la_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99 -g