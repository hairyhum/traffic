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
    {Path, _} = cowboy_req:path(Req),
    lager:error("Path ~p", [Path]),
    case parse_observation(Body) of
      {ok, {SequenceId, {Color, Numbers}}} ->
        case sequence:add_observation(SequenceId, {Color, Numbers}) of
          {ok, {Start, Missing}} ->
            Response = jsonx:encode([{status, ok}, {response, [{start, Start}, {missing, Missing}]}]),
            req:reply(200, Response, Req, State);
          {error, Err} ->
            Response = jsonx:encode([{status, error}, {msg, Err}]),
            req:reply(422, Response, Req, State)
        end;
      {error, Reason} ->
        req:reply(400, Reason, Req, State)
    end
  end).

parse_observation(Data) when not is_list(Data) -> {error, "Data shpuld be json object"};
parse_observation(Data) ->
  SequenceId = proplists:get_value(sequence, Data),
  Observation = proplists:get_value(observation, Data),
  if 
    SequenceId == undefined ->
      {error, "Sequence required"};
    not is_binary(SequenceId) ->
      {error, "Sequence should be string"};
    Observation == undefined ->
      {error, "Observation is required"};
    not is_list(Observation) ->
      {error, "Observation should be json object"};
    true ->
      Color = proplists:get_value(<<"color">>, Observation),
      Numbers = proplists:get_value(<<"numbers">>, Observation),
      if 
        Color == undefined ->
          {error, "Color is required"};
        Color =/= <<"green">>; Color =/= <<"red">> ->
          {error, "Color can be either red or green"};
        Numbers == undefined andalso Color =/= <<"red">> ->
          {error, "Numbers required for green color"};
        Color =/= <<"red">>, not is_list(Numbers) orelse length(Numbers) =/= 2 ->
          {error, "Exaxtly 2 numbers required"};
        true ->
          {SequenceId, {Color, Numbers}}
      end
  end.

