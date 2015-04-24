#!/usr/bin/env sh
APP_NAME = front
PWD=`pwd`
export PATH := /opt/bin:$(PATH)

compile:
	./rebar compile

clean:
	./rebar clean

console:
	erl -pa ebin -pa deps/*/ebin

build_plt:
	ERL_LIBS=$(PWD)/deps dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc mnesia -r deps
analyze: compile 
	ERL_LIBS=$(PWD)/deps dialyzer --plt $(APP_NAME).plt -r -I deps --verbose  ebin
