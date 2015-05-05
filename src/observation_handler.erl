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
    case parse_observation(Body) of
      {ok, {SequenceId, {Color, Numbers}}} ->
        case observation:check_observation(SequenceId, {Color, Numbers}) of
          {ok, {Start, Missing}} ->
            Response = jsonx:encode([{status, ok}, {response, [{start, Start}, {missing, Missing}]}]),
            req:reply(200, Response, Req, State);
          {error, Err} ->
            Response = jsonx:encode([{status, error}, {msg, error_msg(Err)}]),
            req:reply(422, Response, Req, State)
        end;
      {error, Reason} ->
        req:reply(400, Reason, Req, State)
    end
  end).

error_msg(sequence_not_found) -> <<"“The sequence isn't found">>;
error_msg(no_solutions_found) -> <<"No solutions found">>;
error_msg(not_enough_data) -> <<"There isn't enough data">>;
error_msg(red_observation_not_last) -> <<"“The red observation should be the last">>.

parse_observation(Data) when not is_list(Data) -> {error, "Data shpuld be json object"};
parse_observation(Data) ->
  SequenceId = proplists:get_value(<<"sequence">>, Data),
  Observation = proplists:get_value(<<"observation">>, Data),
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
        Color =/= <<"green">>, Color =/= <<"red">> ->
          {error, "Color can be either red or green"};
        Numbers == undefined andalso Color =/= <<"red">> ->
          {error, "Numbers required for green color"};
        Color =/= <<"red">>, not is_list(Numbers) orelse length(Numbers) =/= 2 ->
          {error, "Exaxtly 2 numbers required"};
        true ->
          {ok, {SequenceId, {Color, Numbers}}}
      end
  end.

