PROJECT = tlor
DIALYZER = dialyzer
REBAR = rebar
DEBUG = tlor_debug

.PHONY: all deps compile clean test ct build-plt dialyze release 

all: deps compile

deps:
	$(REBAR) -C rebar.config get-deps

compile:
	$(REBAR) compile -Dtlor_debug

clean:
	$(REBAR) clean
	rm -f apps/tlor/ebin/*
	find . -type f -name "*.dump" | xargs rm -f

test: ct dialyze doc

test-build:
	$(REBAR) -C rebar.test.config compile

ct: clean deps test-build
	$(REBAR) -C rebar.test.config eunit skip_deps=true

build-plt:
	$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib sasl inets 

dialyze: clean deps test-build
	$(DIALYZER) --plt .$(PROJECT).plt ebin

release: compile
	cd rel/; $(REBAR) generate
