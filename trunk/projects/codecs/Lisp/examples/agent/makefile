
RL_GLUE_BASE_PATH = ../..
RL_GLUE_PATH = $(RL_GLUE_BASE_PATH)/src
RL_GLUE_UTILS_PATH = $(RL_GLUE_PATH)/utils

include $(RL_GLUE_PATH)/rl-glue.makefile

AGENT = random-agent
AGENT_PATH = $(RL_GLUE_BASE_PATH)/examples/agent
AGENT_LISP = $(AGENT_PATH)/src/$(AGENT).lisp
AGENT_FASL = $(AGENT_PATH)/build/$(AGENT).fasl

TARGET = RL_agent

.PHONY: all
all: $(TARGET)

$(TARGET): $(AGENT_FASL)
	$(info Creating $@.)
	@$(call lisp_start_sh, $@, \
	        $(call lisp_glue_utils, \
	               $(call lisp_load, "$(AGENT_FASL)") \
	               $(LISP_EVAL) "(rl-random-agent:start-random-agent \
	                               $(RLGLUE_HOST) $(RLGLUE_PORT) \
	                               $(RLGLUE_AUTORECONNECT))" ))
	chmod +x $@

$(AGENT_FASL): $(AGENT_LISP)
	$(info Compiling $<.)
	@$(call lisp_compile, lisp_glue_utils, $<, $@)

.PHONY: clean
clean:
	-rm -f $(TARGET) $(AGENT_FASL) $(AGENT_PATH)/build/$(AGENT).lib

