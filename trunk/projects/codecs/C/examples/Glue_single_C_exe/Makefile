# -------------------------------------------------
# Select the agent, environment and experiment 
# program you wish to compile with rl-glue (into
# one executable. All must be c-files
# -------------------------------------------------
AGENT = RandomAgent.c
AGENT_SRC_PATH = ../Agent/src
ENVIRONMENT = mines.c
ENV_SRC_PATH = ../Environment/src
EXPERIMENT = experiment.c
EXP_SRC_PATH = ../Experiment/src
BUILD_PATH = ./Build
BIN_PATH = .
# -------------------------------------------------

AGENT_OBJECTS = $(basename $(AGENT)).o C_TaskSpec_Parser.o 
ENV_OBJECTS = $(basename $(ENVIRONMENT)).o
EXP_OBJECTS = $(basename $(EXPERIMENT)).o

CFLAGS  = $(EXTRA_CFLAGS) -I$(ENV_SRC_PATH) -I$(AGENT_SRC_PATH) -I$(EXP_SRC_PATH)
LDFLAGS = $(EXTRA_LDFLAGS) -lm -lrlutils -lrlglue

all: $(BIN_PATH)/RL_agent_env_experiment

$(BIN_PATH)/RL_agent_env_experiment: $(BUILD_PATH)/$(basename $(ENVIRONMENT)).o $(BUILD_PATH)/$(basename $(AGENT)).o $(BUILD_PATH)/$(basename $(EXPERIMENT)).o
	$(CC) $(LDFLAGS) $^  -o RL_agent_env_experiment 


$(BUILD_PATH)/$(basename $(ENVIRONMENT)).o: $(ENV_SRC_PATH)/$(ENVIRONMENT)
	$(CC) -c $(CFLAGS) $^ -o $@

$(BUILD_PATH)/$(basename $(AGENT)).o: $(AGENT_SRC_PATH)/$(AGENT)
	$(CC) -c $(CFLAGS) $^ -o $@

$(BUILD_PATH)/$(basename $(EXPERIMENT)).o: $(EXP_SRC_PATH)/$(EXPERIMENT)
	$(CC) -c $(CFLAGS) $^ -o $@

clean:
	rm -rf RL_agent_env_experiment RL_agent_env_experiment.exe $(BUILD_PATH)/*
