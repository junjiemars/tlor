PROJECT = tlor
DIALYZER = dialyzer
REBAR = rebar
OPTIONS = -Dtlor_debug

.PHONY: all deps compile clean distclean test ct build-plt dialyze release 

all: deps compile

rebar-version:
	$(REBAR) --version

deps:
	$(REBAR) -C rebar.config get-deps

compile: deps
	$(REBAR) compile $(OPTIONS)

clean:
	@(cd apps/tlor && $(REBAR) clean)

distclean: clean
	@rm -r rel/tlor
	@find . -type f -name "*.dump" | xargs rm -f

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

fix-exmpp-deps:
	echo "waiting implement"

release: compile
	@(cd rel/ && $(REBAR) generate)
