all: deps
	@./rebar update-deps compile
	@ERL_LIBS=`pwd`/deps escript release/build_rel.escript boot logplex `pwd`/ebin

deps:
	@./rebar get-deps

clean:
	@./rebar clean
