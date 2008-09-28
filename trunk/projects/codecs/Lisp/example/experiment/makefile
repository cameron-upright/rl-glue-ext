
RL_GLUE_BASE_PATH = ../..
TARGETS = RL_experiment

include $(RL_GLUE_BASE_PATH)/makefile.rl-glue

$(TARGETS): $(RL_GLUE_EXAMPLES_TARGET)
	$(call lisp_start_sh, $@, \
	       $(call lisp_run, \
		   $(call asdf_load, rl-glue-clcdc) \
		   $(call asdf_load, rl-glue-utils) \
		   $(call asdf_load, rl-glue-examples) \
	       $(LISP_EVAL) "(rl-episode-avg:start-episode-avg \
	                       10 $(RLGLUE_HOST) $(RLGLUE_PORT))" ))
	chmod +x $@

.PHONY: clean
clean:
	-rm -f $(TARGETS)

