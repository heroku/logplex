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

oldtest: REBAR := $(REBAR),test
oldtest: oldtestclean
	$(REBAR) release
	ERL_LIBS=$(ROOT_DIR)/_build/public+test/lib/:${ERL_LIBS} ct_run -spec logplex.spec

# `mix ct` invokes the test as https://github.com/alco/mix-erlang-tasks/blob/master/lib/mix/tasks/ct.ex
test:
	elixir --name logplex@testnode -S mix ct

clean:
	$(REBAR) clean
	rm -rf ./_build/
	rm -f erl_crash.dump

oldtestclean:
	$(REBAR) clean
	rm -rf ./_build/test

distclean: clean
