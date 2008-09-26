# ------------------------------------------------------------------
# Changes these values to use different source files and different 
# directories 
# ------------------------------------------------------------------
AGENT = RandomAgent
AGENT_SRC_PATH = ./src
BUILD_PATH = ./Build
# ------------------------------------------------------------------



all: RL_agent


ECHOFLAG = -e

ifeq ($(shell uname -s),Darwin)
	ECHOFLAG = 
endif

RL_agent:
	echo $(ECHOFLAG) "#! /bin/bash\n PYTHONPATH=../../../codecs/Python/:$(AGENT_SRC_PATH) python -c \"import rlglue.agent.AgentLoader\" $(AGENT)" | cat > RL_agent
	chmod u+x RL_agent

clean:
	rm -f RL_agent *.pyc src/*.pyc 
	rm -f -r src/$(AGENT).app