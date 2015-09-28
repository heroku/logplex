REBAR?=./rebar3 as public

LOGPLEX_PLT=$(CURDIR)/.depsolver_plt

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: clean distclean test

compile:
	$(REBAR) update
	$(REBAR) release

dist: REBAR := $(REBAR),prod
dist: compile
	$(REBAR) tar

update:
	$(REBAR) update

# dialyzer:
# 	@./rebar3 dialyzer
#
# typer: $(LOGPLEX_PLT)
# 	typer --plt $(HERMES_PLT) -I deps/ -r src

test: REBAR := $(REBAR),test
test: testclean
	$(REBAR) release
	ERL_LIBS=$(ROOT_DIR)/_build/public+test/lib/:${ERL_LIBS} ct_run -spec logplex.spec

clean:
	$(REBAR) clean
	rm -rf ./_build/
	rm -f erl_crash.dump

testclean:
	$(REBAR) clean
	rm -rf ./_build/test

distclean: clean
