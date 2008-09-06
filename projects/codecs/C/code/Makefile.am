CODEC_RELEASE=1:0:0
CODEC_VERSION=1:0:0

lib_LTLIBRARIES = librlutils.la librlagent.la librlenvironment.la librlexperiment.la
 
librlutils_la_SOURCES = src/TaskSpec_Parser.c src/RLStruct_util.c rlglue/utils/C/TaskSpec_Parser.h rlglue/utils/C/RLStruct_util.h 

librlutils_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
librlutils_la_includedir=$(includedir)/rlglue/utils/C
librlutils_la_include_HEADERS = rlglue/utils/C/TaskSpec_Parser.h rlglue/utils/C/RLStruct_util.h

librlagent_la_SOURCES = src/RL_client_agent.c src/RL_network.c RL_network.h
librlagent_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)

librlenvironment_la_SOURCES = src/RL_client_environment.c src/RL_network.c RL_network.h
librlenvironment_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)

librlexperiment_la_SOURCES = src/RL_client_experiment.c src/RL_network.c RL_network.h
librlexperiment_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
