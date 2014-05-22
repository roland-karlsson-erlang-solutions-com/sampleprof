all: compile

compile: deps
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

clean-all: clean
	rm -rf deps
