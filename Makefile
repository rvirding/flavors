# Copyright (c) 2016-2020 Robert Virding
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
#  limitations under the License.

# Makefile for LFE Flavors

EBINDIR = ebin
SRCDIR = src
LSRCDIR = src
INCDIR = include

VPATH = $(SRCDIR)

ERLCFLAGS = -W1 +debug_info
ERLC = erlc

LFECFLAGS = +debug-info
LFEC = lfec
APP_DEF = flavors.app

LIB=lfe

# To run erl as bash
FINISH=-run init stop -noshell

## The .erl, .lfe and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS = $(notdir $(wildcard $(SRCDIR)/*.xrl))
YSRCS = $(notdir $(wildcard $(SRCDIR)/*.yrl))
LSRCS = $(notdir $(wildcard $(LSRCDIR)/*.lfe))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam) $(YSRCS:.yrl=.beam)
LBINS = $(LSRCS:.lfe=.beam)

.SUFFIXES: .erl .lfe .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(HAS_MAPS) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

%.erl: %.yrl
	$(ERLC) -o $(SRCDIR) $<

$(EBINDIR)/%.beam: $(LSRCDIR)/%.lfe
	$(LFEC) -I $(INCDIR) -o $(EBINDIR) -pa ebin $(LFECFLAGS) $<

all: compile

.PHONY: compile erlc-compile lfec-compile erlc-lfec install docs clean

compile: 
	$(MAKE) $(MFLAGS) erlc-lfec

## Compile Erlang files using erlc and LFE files using lfec
erlc-lfec: erlc-compile $(EBINDIR)/$(APP_DEF) lfec-compile

## Compile using erlc
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS))

## Compile LFE files using lfec
lfec-compile: $(addprefix $(EBINDIR)/, $(LBINS))

$(addprefix $(EBINDIR)/, $(LBINS)): $(addprefix $(EBINDIR)/, $(EBINS))

$(EBINDIR)/$(APP_DEF): $(SRCDIR)/$(APP_DEF).src
	cp $(SRCDIR)/$(APP_DEF).src $(EBINDIR)/$(APP_DEF)

clean:
	rm -rf $(EBINDIR)/*.beam erl_crash.dump

echo:
	@ echo $(ESRCS)
	@ echo $(LSRCS)
	@ echo $(EBINS)

# Targets for generating docs and man pages
DOCDIR = doc
DOCSRC = $(DOCDIR)/src
PDFSRC = $(DOCDIR)/pdf

docs:
