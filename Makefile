 # Makefile for LFE
# This simple Makefile uses rebar (in Unix) or rebar.cmd (in Windows)
# to compile/clean if it exists, else does it explicitly.

EBINDIR = ebin
SRCDIR = src
LSRCDIR = src
INCDIR = include
DOCDIR = doc

VPATH = $(SRCDIR)

ERLCFLAGS = -W1
ERLC = erlc
LFEC = lfec

LIB=lfe

## The .erl, .lfe and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS = $(notdir $(wildcard $(SRCDIR)/*.xrl))
YSRCS = $(notdir $(wildcard $(SRCDIR)/*.yrl))
LSRCS = $(notdir $(wildcard $(LSRCDIR)/*.lfe))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam) $(YSRCS:.yrl=.beam)
LBINS = $(LSRCS:.lfe=.beam)

.SUFFIXES: .erl .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(HAS_MAPS) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

%.erl: %.yrl
	$(ERLC) -o $(SRCDIR) $<

$(EBINDIR)/%.beam: $(LSRCDIR)/%.lfe
	$(LFEC) -I $(INCDIR) -o $(EBINDIR) -pa ebin $<

all: compile docs

.PHONY: compile erlc-compile lfec-compile install docs clean

## Compile using rebar if it exists else using make
compile:
	if which rebar.cmd > /dev/null; \
	then rebar.cmd compile; \
	elif which rebar > /dev/null; \
	then rebar compile; \
	else \
	$(MAKE) $(MFLAGS) erlc-compile; \
	$(MAKE) $(MFLAGS) lfec-compile; \
	fi

## Compile using erlc
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS))

## Compile using lfec
lfec-compile: $(addprefix $(EBINDIR)/, $(LBINS))

erlc-lfec: erlc-compile lfec-compile

docs:

clean:
	if which rebar.cmd > /dev/null; \
	then rebar.cmd clean; \
	elif which rebar > /dev/null; \
	then rebar clean; \
	else rm -rf $(EBINDIR)/*.beam; \
	fi
	rm -rf erl_crash.dump

echo:
	@ echo $(ESRCS)
	@ echo $(EBINS)
