default: main
.PHONY: default

ifndef PUREC_DIR
$(error '$$PUREC_DIR not set')
endif

export PATH := $(PUREC_DIR)/node_modules/.bin:$(PATH)
include $(PUREC_DIR)/mk/target.mk

SHELL := /bin/bash
srcs := src
deps := $(shell\
	find bower_components/purescript-{ordered-collections,gen,transformers,proxy,typelevel-prelude,arrays,control,assert,effect,prelude,console,functions,identity,either,integers,bifunctors,orders,newtype,type-equality,math,distributive,refs,unsafe-coerce,st,lazy,foldable-traversable,unfoldable,partial,tuples,maybe,newtype,invariant,tailrec,nonempty}/src/\
		-type f\
		-name '*.purs')
$(eval $(call purs_mk_target,main,Main,$(srcs),$(deps)))
