all:
	@./rebar get-deps update-deps compile
	@ERL_LIBS=`pwd`/deps escript release/build_rel.escript boot logplex `pwd`/ebin

clean:
	@./rebar clean
