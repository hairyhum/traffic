-module(req).

-export([reply/3, reply/4, reply/5, with_json_body/3]).


-type status() :: cowboy:http_status().
-type body() :: iodata().
-type headers() :: cowboy:http_headers().
-type req() :: cowboy_req:req().

-spec reply(status(), req(), State) -> {ok, req(), State}.
reply(Status, Req, State) ->
  {ok, Req1} = cowboy_req:reply(Status, Req),
  {ok, Req1, State}.

-spec reply(status(), body(), req(), State) -> {ok, req(), State}.
reply(Status, Body, Req, State) ->
  {ok, Req1} = cowboy_req:reply(Status, [], Body, Req),
  {ok, Req1, State}.

-spec reply(status(), headers(), body(), req(), State) -> {ok, req(), State}.
reply(Status, Headers, Body, Req, State) ->
  {ok, Req1} = cowboy_req:reply(Status, Headers, Body, Req),
  {ok, Req1, State}.


-spec with_json_body(Req, term(), fun((list()) -> Resp)) -> Resp
  when Req  :: req(),
       Resp :: {ok, Req, term()}.
with_json_body(Req, State, Fun) ->
  {ok, Body, _} = cowboy_req:body(Req),
  case json:parse(Body) of
    {incomplete, _} ->  reply(400, <<"\"Invalid json\"">>, Req, State);
    {error,_,_} -> reply(400, <<"\"Invalid json\"">>, Req, State);
    InData ->
      Fun(InData)
  end.
