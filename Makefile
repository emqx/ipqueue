.PHONY: all
all:
	rebar3 do compile, dialyzer, eunit
	rebar3 as test proper
