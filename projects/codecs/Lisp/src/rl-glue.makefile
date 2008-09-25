
LISP ?= $(shell \
	      which sbcl || \
	      which lisp || \
	      which clisp || \
	      which ccl)

LISP_PATH ?= \
	/usr/share/common-lisp/source/asdf/ \
	/usr/share/common-lisp/systems/

LISP_EXEC = $(basename $(notdir $(LISP)))
LISP_FORMATTED_PATH = $(patsubst %,\#P\"%\",$(LISP_PATH))

ifeq ($(LISP_EXEC),sbcl) # SBCL
LISP += --noinform
LISP_EVAL = --eval
LISP += $(LISP_EVAL) "(require 'asdf)"
else
ifeq ($(LISP_EXEC),lisp) # CMUCL
LISP += -quiet
LISP_EVAL = -eval
LISP += $(LISP_EVAL) "(require 'asdf)"
else
ifeq ($(LISP_EXEC),clisp) # CLISP
LISP += -q -q -K full
LISP_EVAL = -x
LISP += $(LISP_EVAL) "(setf custom:*load-paths* \
	                        (append custom:*load-paths* \
	                                (list $(LISP_FORMATTED_PATH))))"
LISP += $(LISP_EVAL) "(require \"asdf\")"
else
ifeq ($(LISP_EXEC),ccl) # openMCL
LISP_EVAL = --eval
LISP += $(LISP_EVAL) "(require 'asdf)"
else
$(error Not supported lisp implementation : $(LISP_EXEC))
endif
endif
endif
endif

###############################################################################
### Initializing ASDF central registry.

LISP += \
	$(LISP_EVAL) "(setf asdf:*central-registry* \
	                    (append (list \#P\"$(RL_GLUE_PATH)/\" \
	                                  \#P\"$(RL_GLUE_UTILS_PATH)/\" \
	                                  $(LISP_FORMATTED_PATH)) \
	                            asdf:*central-registry*))"

###############################################################################
### Loading systems and lisp files.

lisp_load = $(LISP_EVAL) "(load $(call quote,$(1)))"

asdf_operate = $(LISP_EVAL) "(asdf:operate '$(1) '$(2)"
asdf_load = $(call asdf_operate,asdf:load-op,$(1))
asdf_compile = $(call asdf_operate,asdf:compile-op,$(1))

LISP_RL_GLUE = $(call asdf_load,rl-glue)
LISP_RL_GLUE_UTILS = $(call asdf_load,rl-glue-utils)
LISP_QUIT = $(LISP_EVAL) "(quit)"

###############################################################################
### Running Lisp and compilation.

lisp_run = $(LISP) $(1) $(2) $(LISP_QUIT)
lisp_glue = $(call lisp_run,$(LISP_RL_GLUE),$(1))
lisp_glue_utils = $(call lisp_run,$(LISP_RL_GLUE) $(LISP_RL_GLUE_UTILS),$(1))

fasl_realpath = "$(realpath $(dir $(strip $(1))))/$(notdir $(strip $(1)))"

lisp_compile = \
	$(call $(1), $(LISP_EVAL) \
	"(compile-file $(call quote,"$(strip $(2))") \
	               :output-file $(call quote,$(call fasl_realpath, $3)))")

###############################################################################
### Lisp starter shell script creation.

shvarname = `echo -e \$${$(1)}`

RLGLUE_HOST = $(call shvarname,RLGLUE_HOST)
RLGLUE_HOST_SH =\
	if [ -n "$(RLGLUE_HOST)" ]; then\
	RLGLUE_HOST=" :host \"$(RLGLUE_HOST)\"";\
	fi\n

RLGLUE_PORT = $(call shvarname,RLGLUE_PORT)
RLGLUE_PORT_SH =\
	if [ -n "$(RLGLUE_PORT)" ]; then\
	RLGLUE_PORT=" :port $(RLGLUE_PORT)";\
	fi\n

RLGLUE_AUTORECONNECT = $(call shvarname,RLGLUE_AUTORECONNECT)
RLGLUE_AUTORECONNECT_SH =\
	if [ -n "$(RLGLUE_AUTORECONNECT)" ]; then\
	RLGLUE_AUTORECONNECT=" :autoreconnect $(RLGLUE_AUTORECONNECT)";\
	fi\n

get_glue_param = $(if $(findstring $(1),$(2)),$(call quote,$($(1)_SH)))

quote = $(subst ",\",$(subst \,\\,$(1)))
lisp_start_sh = \
	$(shell echo -e "\#! /bin/sh\n\n\
	$(call get_glue_param,RLGLUE_HOST,$(2))\
	$(call get_glue_param,RLGLUE_PORT,$(2))\
	$(call get_glue_param,RLGLUE_AUTORECONNECT,$(2))\
	\n$(call quote,$(2))" > $(1))

