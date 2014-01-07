DIALYZER_OPTS = -Wrace_conditions -Wunderspecs

all:
	rebar compile

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
ERLANG_APPS=erts kernel stdlib 

$(DEPSOLVER_PLT):
		dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
					--apps $(ERLANG_APPS) 

dialyzer: $(DEPSOLVER_PLT)
		dialyzer --plt $(DEPSOLVER_PLT) $(DIALYZER_OPTS) --src src

typer: $(DEPSOLVER_PLT)
		typer --plt $(DEPSOLVER_PLT) -r ./src

clean:
	rebar clean

.PHONY: dialyzer typer clean
