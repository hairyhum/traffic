-module(sequence_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

terminate(_,_,_) -> ok.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  case Method of
    <<"POST">> -> new_sequence(Req, State);
    _ -> req:reply(405, Req, State)
  end.

new_sequence(Req, State) ->
  {ok, SequenceId} = observation:new_sequence(),
  Response = jsonx:encode([{status, ok}, {sequence, SequenceId}]),
  req:reply(200, Response, Req, State).