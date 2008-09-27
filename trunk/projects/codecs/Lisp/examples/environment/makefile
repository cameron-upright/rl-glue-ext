
RL_GLUE_BASE_PATH = ../..
RL_GLUE_CLCDC_PATH = $(RL_GLUE_BASE_PATH)/rl-glue-clcdc
RL_GLUE_UTILS_PATH = $(RL_GLUE_BASE_PATH)/rl-glue-utils

include $(RL_GLUE_BASE_PATH)/makefile.rl-glue

ENV = mines
ENV_PATH = $(RL_GLUE_BASE_PATH)/examples/environment
ENV_LISP = $(ENV_PATH)/$(ENV).lisp
ENV_FASL = $(ENV_PATH)/$(ENV).fasl

TARGET = RL_environment

.PHONY: all
all: $(TARGET)

$(TARGET): $(ENV_FASL)
	$(info Creating $@.)
	@$(call lisp_start_sh, $@, \
	        $(call lisp_glue, \
	               $(call lisp_load, "$(ENV_FASL)") \
	               $(LISP_EVAL) "(rl-mines:start-mines \
	                               $(RLGLUE_HOST) $(RLGLUE_PORT) \
	                               $(RLGLUE_AUTORECONNECT))" ))
	chmod +x $@

$(ENV_FASL): $(ENV_LISP)
	$(info Compiling $<.)
	@$(call lisp_compile, lisp_glue, $<, $@)

.PHONY: clean
clean:
	-rm -f $(TARGET) $(ENV_FASL) $(ENV_PATH)/$(ENV).lib

