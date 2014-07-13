ERLC_FLAGS=-Wall -I include

ERL_FLAGS=-pa ebin

SOURCES=$(wildcard src/*.erl)

HEADERS=$(wildcard include/*.hrl)

OBJECTS:=$(SOURCES:src/%.erl=ebin/%.beam)


APPNAME=Simple Gnutella - P2P (1DT048) 2014


USER:=$(shell whoami)
# USER=abcd1234
HOST:=$(shell hostname | cut -d '.' -f1)

SUFFIX=_$(USER)

LOGGER:=logger$(SUFFIX)
CACHE:=cache$(SUFFIX)

CLI:=cli_$(NODE)$(SUFFIX)

.PHONY: doc doc_url skeleton


all: $(OBJECTS)

ebin/%.beam: src/%.erl $(HEADERS)
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	rm -Rf ebin/*
	rm -Rf src/*.beam
	rm -Rf doc/*.html

simple: ebin/simple.beam

doc:
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"."' '[{def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'


doc_url:
	@echo
	@echo "EDoc index page available at file://$(PWD)/doc/index.html"
	@echo


# Start a dummy Erlang node.
start_test: all
	erl $(ERL_FLAGS) -sname test$(SUFFIX)

# Only start the logger Erlang node, don't start any processes at the
# node.
start_logger: all
	erl $(ERL_FLAGS) -sname $(LOGGER)

# Only start the cache.
start_cache: all
	erl $(ERL_FLAGS) -sname $(CACHE) -eval 'cache:start()'




# Start the cache and the peers south and north.
setup: all
	erl $(ERL_FLAGS) -sname $(LOGGER) -eval 'setup:system([south, north, west, east])'


# NOTE: You must set the variable NODE before calling any of these targets.

# NOTE: start_node should be called from the logger node.
start_node:  $(OBJECTS)
	erl $(ERL_FLAGS) -sname $(NODE) -eval 'setup:node($(NODE))'
	erl $(ERL_FLAGS) -sname $(CLI) -eval 'setup:node($(NODE))'

start_cli: all
	erl $(ERL_FLAGS) -noshell -sname $(CLI) -eval 'setup:cli($(NODE), $(LOGGER)@$(HOST))'


# START SKIP LINES

SKELETON_DIR=skeleton
SKELETON_NAME=osm_2013_problem_set_3

SKELETON=$(SKELETON_DIR)/$(SKELETON_NAME)

SKELETON_ARCHIVE=$(SKELETON_NAME).tar.gz
.ONESHELL:
skeleton: clean
	rm -rf $(SKELETON)/*
	mkdir $(SKELETON)/src
	mkdir $(SKELETON)/doc
	mkdir $(SKELETON)/ebin
	./filter -d $(SKELETON) Makefile src/*.erl
	cp help $(SKELETON)/.
	cp doc/overview.edoc $(SKELETON)/doc/.
	cp doc/my_style.css $(SKELETON)/doc/.
	cp doc/uu.png $(SKELETON)/doc/.
	cd $(SKELETON) &&  make clean
	cd $(SKELETON_DIR) &&  tar cvfz ../$(SKELETON_ARCHIVE) $(SKELETON_NAME)

# END SKIP LINES
