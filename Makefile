#!/usr/bin/env sh
APP_NAME = front
PWD=`pwd`
export PATH := /opt/bin:$(PATH)

deps:
	./rebar get_deps

compile:
	./rebar compile

clean:
	./rebar clean

console:
	erl -pa ebin -pa deps/*/ebin -s myapp_app start

eunit:
	./rebar eunit skip_deps=true

common_test:
	./rebar ct skip_deps=true

tests: common_test eunit

build_plt:
	ERL_LIBS=$(PWD)/deps dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc mnesia -r deps
analyze: compile 
	ERL_LIBS=$(PWD)/deps dialyzer --plt $(APP_NAME).plt -r -I deps --verbose  ebin

