#!/usr/bin/env sh
APP_NAME = traffic_light
PWD=`pwd`
export PATH := /opt/bin:$(PATH)

deps:
	./rebar get-deps

compile: deps
	./rebar compile

clean:
	./rebar clean

console:
	erl -sname traffic_light@localhost -pa ebin -pa deps/*/ebin -s traffic_light_app start

start:
	erl -sname traffic_light@localhost -detached -pa ebin -pa deps/*/ebin -s traffic_light_app start

stop:
	./stop.escript traffic_light@localhost

eunit:
	./rebar eunit skip_deps=true

common_test:
	./rebar ct skip_deps=true

tests: compile common_test eunit

build_plt:
	ERL_LIBS=$(PWD)/deps dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc mnesia -r deps
analyze: compile 
	ERL_LIBS=$(PWD)/deps dialyzer --plt $(APP_NAME).plt -r -I deps --verbose  ebin

