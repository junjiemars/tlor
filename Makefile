PROJECT = tlor
DIALYZER = dialyzer
REBAR = rebar
OPTIONS = -Dtlor_debug

.PHONY: all deps compile clean test ct build-plt dialyze release 

all: deps compile

deps:
	$(REBAR) -C rebar.config get-deps

compile:
	$(REBAR) compile $(OPTIONS)

clean:
	$(REBAR) clean
	rm -r apps/tlor/ebin/
	rm -r rel/tlor/releases/
	rm -r rel/tlor/lib/
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
