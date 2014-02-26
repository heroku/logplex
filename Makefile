.PHONY: run compile build deps clean distclean test docs

ERL=`which erl`
REBAR="./rebar"
HOSTNAME=`hostname --fqdn`

run: compile
	exec ${PWD}/bin/devel_logplex

compile:
	${REBAR} -C public.rebar.config compile skip_deps=true

build: deps
	${REBAR} -C public.rebar.config compile

deps:
	test -d deps || ${REBAR} get-deps -C public.rebar.config

clean:
	${REBAR} -C public.rebar.config clean

distclean: clean
	${REBAR} delete-deps
	rm -r ${PWD}/deps

test:
	${REBAR} -C public.rebar.config compile eunit skip_deps=true

docs:
	${REBAR} -C public.rebar.config doc skip_deps=true

help:
	@echo "Targets: run, compile, build, deps, clean, distclean, test and docs"