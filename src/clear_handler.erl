-module(clear_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

terminate(_,_,_) -> ok.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  case Method of
    <<"GET">> -> clear_data(Req, State);
    _ -> req:reply(405, Req, State)
  end.


clear_data(Req, State) ->
  ok = observation:clear(),
  Response = jsonx:encode([{status, ok}, {response, ok}]),
  req:reply(200, Response, Req, State).