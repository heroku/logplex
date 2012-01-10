all: deps
	@./rebar update-deps compile
	@escript release/build_rel.escript boot logplex `pwd`/ebin

deps:
	@./rebar get-deps

clean:
	@./rebar clean
