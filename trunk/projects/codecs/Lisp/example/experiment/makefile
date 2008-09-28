
RL_GLUE_BASE_PATH = ../..
include $(RL_GLUE_BASE_PATH)/makefile.rl-glue

EXP = experiment
EXP_PATH = $(RL_GLUE_EXAMPLE_PATH)/experiment
EXP_LISP = $(EXP_PATH)/$(EXP).lisp
EXP_FASL = $(EXP_PATH)/$(EXP).fasl

TARGET = RL_experiment

.PHONY: all
all: $(TARGET)

$(TARGET): $(EXP_FASL)
	$(info Creating $@.)
	@$(call lisp_start_sh, $@, \
	        $(call lisp_glue, \
	               $(call lisp_load, "$(EXP_FASL)") \
	               $(LISP_EVAL) "(rl-episode-avg:start-episode-avg \
	                               10 $(RLGLUE_HOST) $(RLGLUE_PORT))" ))
	chmod +x $@

$(EXP_FASL): $(EXP_LISP)
	$(info Compiling $<.)
	@$(call lisp_compile, lisp_glue, $<, $@)

.PHONY: clean
clean:
	-rm -f $(TARGET) $(EXP_FASL) $(EXP_PATH)/$(EXP).lib

