.PHONY: all compile quick get-deps dist-clean

REBAR := ./rebar

all : clean compile

compile : get-deps
	@$(REBAR) compile

quick :
	@$(REBAR) compile skip_deps=true

get-deps :
	@$(REBAR) get-deps

clean :
	@$(REBAR) clean

dist-clean : clean
	@rm -rf deps ebin
