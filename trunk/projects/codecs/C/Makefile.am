CODEC_RELEASE=1:0:0
CODEC_VERSION=1:0:0

lib_LTLIBRARIES = librlagent.la librlenvironment.la librlexperiment.la
 

librlagent_la_SOURCES = tmp/RL_client_agent.c tmp/RL_network.c tmp/C_TaskSpec_Parser.c tmp/RLStruct_util.c rlglue/codecs/C/C_TaskSpec_Parser.h rlglue/codecs/C/RLStruct_util.h  RL_network.h

librlagent_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
librlagent_la_includedir=$(includedir)/rlglue/codecs/C
librlagent_la_include_HEADERS = rlglue/codecs/C/C_TaskSpec_Parser.h rlglue/codecs/C/RLStruct_util.h

librlenvironment_la_SOURCES = tmp/RL_client_environment.c tmp/RL_network.c tmp/C_TaskSpec_Parser.c tmp/RLStruct_util.c rlglue/codecs/C/RLStruct_util.h RL_network.h
librlenvironment_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)

librlexperiment_la_SOURCES = tmp/RL_client_experiment.c tmp/RL_network.c tmp/C_TaskSpec_Parser.c tmp/RLStruct_util.c rlglue/codecs/C/RLStruct_util.h RL_network.h
librlexperiment_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
