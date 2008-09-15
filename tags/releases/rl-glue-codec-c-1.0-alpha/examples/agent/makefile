# ------------------------------------------------------------------
# Changes these values to use different source files and different 
# directories 
# ------------------------------------------------------------------
AGENT = RandomAgent.c
AGENT_SRC_PATH = ./src
BUILD_PATH = ./Build
# ------------------------------------------------------------------
AGENT_OBJECTS = $(basename $(AGENT)).o
CFLAGS = -I$(AGENT_SRC_PATH)

all: RL_agent

RL_agent: $(addprefix $(BUILD_PATH)/, $(AGENT_OBJECTS))  
	$(CC)   -lrlagent -lrlutils -lm -o RL_agent $^     

$(BUILD_PATH)/$(basename $(AGENT)).o: $(AGENT_SRC_PATH)/$(AGENT)
	$(CC) -c $(CFLAGS) $^ -o $@ 

clean:
	rm -rf RL_agent RL_agent.exe $(BUILD_PATH)/*









