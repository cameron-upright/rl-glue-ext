# ------------------------------------------------------------------
# Changes these values to use different source files and different 
# directories 
# ------------------------------------------------------------------
ENVIRONMENT = mines.c
ENV_SRC_PATH = ./src
BUILD_PATH = ./Build
# ------------------------------------------------------------------

ENV_OBJECTS = $(basename $(ENVIRONMENT)).o 

CFLAGS = -I$(ENV_SRC_PATH)

all: RL_environment

RL_environment: $(addprefix $(BUILD_PATH)/, $(ENV_OBJECTS))
	$(CC)  -lrlenvironment -lm -o RL_environment $^

$(BUILD_PATH)/$(basename $(ENVIRONMENT)).o: $(ENV_SRC_PATH)/$(ENVIRONMENT)
	$(CC) 	-c $(CFLAGS) $^ -o $@

clean:
	rm -rf RL_environment RL_environment.exe $(BUILD_PATH)/*
