# ------------------------------------------------------------------
# Changes these values to use different source files and different 
# directories 
# ------------------------------------------------------------------
EXPERIMENT = experiment.c
EXP_SRC_PATH = ./src
BUILD_PATH = ./Build
# ------------------------------------------------------------------

EXP_OBJECTS = $(basename $(EXPERIMENT)).o

CFLAGS = -I$(EXP_SRC_PATH)

all: RL_experiment
#Note -lrlgluenetdev is only necessary for cygwin compatibility

RL_experiment: $(addprefix $(BUILD_PATH)/,$(EXP_OBJECTS)) 
	$(CC) $^ -lrlexperiment -lrlgluenetdev -lm -o RL_experiment 

$(BUILD_PATH)/$(basename $(EXPERIMENT)).o: $(EXP_SRC_PATH)/$(EXPERIMENT)
	$(CC) -c $(CFLAGS) $^ -o $@

clean:
	rm -rf RL_experiment RL_experiment.exe $(BUILD_PATH)/*
