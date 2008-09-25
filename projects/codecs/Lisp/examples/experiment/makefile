
RL_GLUE_BASE_PATH = ../..
RL_GLUE_PATH = $(RL_GLUE_BASE_PATH)/src
RL_GLUE_UTILS_PATH = $(RL_GLUE_BASE_PATH)/utils

include $(RL_GLUE_PATH)/rl-glue.makefile

EXP = experiment
EXP_PATH = $(RL_GLUE_BASE_PATH)/examples/experiment
EXP_LISP = $(EXP_PATH)/src/$(EXP).lisp
EXP_FASL = $(EXP_PATH)/build/$(EXP).fasl

TARGET = RL_experiment

.PHONY: all
all: $(TARGET)

$(TARGET): $(EXP_FASL)
	$(info Creating $@.)
	@$(call lisp_start_sh, $@, \
	        $(call lisp_glue, \
	               $(call lisp_load, "$(EXP_FASL)") \
	               $(LISP_EVAL) "(rl-episode-avg:start-episode-avg \
	                               1 $(RLGLUE_HOST) $(RLGLUE_PORT))" ))
	chmod +x $@

$(EXP_FASL): $(EXP_LISP)
	$(info Compiling $<.)
	@$(call lisp_compile, lisp_glue, $<, $@)

.PHONY: clean
clean:
	-rm -f $(TARGET) $(EXP_FASL) $(EXP_PATH)/build/$(EXP).lib

