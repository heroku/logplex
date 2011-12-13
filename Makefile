all: deps
	@rebar compile
	@escript release/build_rel.escript boot logplex `pwd`/ebin

deps:
	@rebar get-deps update-deps

clean:
	@rebar clean
