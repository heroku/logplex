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

test:
test: testclean compile
	epmd &
	$(REBAR),test ct --setcookie "asfasfas" --name foo@127.0.0.1
	$(REBAR),test,syslog ct --setcookie "asfasfas" --name foo@127.0.0.1

clean:
	$(REBAR) clean
	rm -rf ./_build/
	rm -f erl_crash.dump

testclean:
	$(REBAR) clean
	rm -rf ./_build/test

distclean: clean
