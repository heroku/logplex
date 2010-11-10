all: 
	@erl -make
	@escript release/build_rel.escript boot logplex `pwd`/ebin

clean:
	rm -f ebin/*.beam