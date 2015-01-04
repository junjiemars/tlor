PROJECT = tlor
DIALYZER = dialyzer
REBAR = rebar
OPTIONS = -Dtlor_debug
RM_EBIN = apps/tlor/ebin/
RM_TLOR = rel/tlor/

.PHONY: all deps compile clean test ct build-plt dialyze release 

all: deps compile

rebar-version:
	$(REBAR) --version

deps:
	$(REBAR) -C rebar.config get-deps

compile: deps
	$(REBAR) compile $(OPTIONS)

clean:
	$(REBAR) clean
	if test -d $(RM_EBIN); then \
		rm -r $(RM_EBIN); \
	fi
	if test -d $(RM_TLOR); then \
		rm -r $(RM_TLOR); \
	fi
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

fix-exmpp-deps:
	echo "waiting implement"

release: compile
	cd rel/; $(REBAR) generate
