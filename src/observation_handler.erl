-module(observation_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

terminate(_,_,_) -> ok.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  case Method of
    <<"POST">> -> add_observation(Req, State);
    _ -> req:reply(405, Req, State)
  end.

add_observation(Req, State) ->
  req:with_json_body(Req, State, fun(Body) ->
    Sequence = proplists:get_value(<<"sequence">>, Body),
    Observation = proplists:get_value(<<"observation">>, Body),
    Color = proplists:get_value(<<"color">>, Observation),
    Numbers = proplists:get_value(<<"numbers">>, Observation),
    
    if Sequence == undefined; Color == undefined; Numbers == undefined ->
      req:reply(400, <<"invalid input data">>, Req, State);
    true ->
      {ok, {Start, Missing}} = sequence:check_observation(Sequence, {Color, Numbers}),
      Response = jsonx:encode([{status, ok}, {response, [{start, Start}, {missing, Missing}]}]),
      req:reply(200, Response, Req, State)
    end
  end).