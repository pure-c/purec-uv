default: main
.PHONY: default

ifndef PUREC_DIR
$(error '$$PUREC_DIR not set')
endif

include $(PUREC_DIR)/mk/target.mk

LD_FLAGS += -luv

$(eval $(call purs_mk_target,main,Test.Main,src test))

check: main
	@./main.out
