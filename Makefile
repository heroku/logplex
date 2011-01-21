all: 
	(cd deps/redis_pool;$(MAKE) all)
	(cd deps/mochiweb;$(MAKE) all)
	(cd deps/pagerduty;$(MAKE) all)
	(cd deps/gen_nb_server;$(MAKE) all)
	@erl -make
	@escript release/build_rel.escript boot logplex `pwd`/ebin

clean:
	rm -f ebin/*.beam erl_crash.dump release/*.boot

clean_all: clean
	(cd deps/redis_pool;$(MAKE) clean)
	(cd deps/mochiweb;$(MAKE) clean)
	(cd deps/pagerduty;$(MAKE) clean)
	(cd deps/gen_nb_server;$(MAKE) clean)
