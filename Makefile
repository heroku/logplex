REBAR := ./rebar3

.PHONY: clean distclean test

compile:
	@$(REBAR) update
	@$(REBAR) release

REBAR += as prod
dist: compile
	@$(REBAR) tar

update:
	@$(REBAR) update

# dialyzer:
# 	@./rebar3 dialyzer
#
# typer: $(HERMES_PLT)
# 	typer --plt $(HERMES_PLT) -I deps/ -r src

test:
	@$(REBAR) ct

clean:
	$(REBAR) clean
	rm -rf ./_build/
	rm -f erl_crash.dump

testclean: clean

distclean: clean
