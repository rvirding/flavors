# Makefile for LFE
# This simple Makefile uses rebar (in Unix) or rebar.cmd (in Windows)
# to compile/clean if it exists, else does it explicitly.

EBINDIR = ebin
SRCDIR = src
INCDIR = include
DOCDIR = doc

VPATH = $(SRCDIR)

ERLCFLAGS = -W1
ERLC = erlc

LIB=lfe

## The .erl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam) $(YSRCS:.yrl=.beam)

.SUFFIXES: .erl .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(HAS_MAPS) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

%.erl: %.yrl
	$(ERLC) -o $(SRCDIR) $<

all: compile docs

.PHONY: compile erlc_compile install docs clean

## Compile using rebar if it exists else using make
compile:
	if which rebar.cmd > /dev/null; \
	then rebar.cmd compile; \
	elif which rebar > /dev/null; \
	then rebar compile; \
	else $(MAKE) $(MFLAGS) erlc_compile; \
	fi

## Compile using erlc
erlc_compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

docs:

clean:
	if which rebar.cmd > /dev/null; \
	then rebar.cmd clean; \
	elif which rebar > /dev/null; \
	then rebar clean; \
	else rm -rf $(EBINDIR)/*.beam; \
	fi
	rm maps.mk
	rm -rf erl_crash.dump

echo:
	@ echo $(ESRCS)
	@ echo $(EBINS)
