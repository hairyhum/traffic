-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  lager:start(),
  application:start(crypto),
  application:start(sasl),
  application:start(ranch),
  application:start(cowlib),
  application:start(cowboy),
  application:start(myapp).


start(_StartType, _StartArgs) ->
  storage:start(),
  Dispatch = cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, Opts})}
    {'_', [
      {"/sequence/create", sequence_handler, []},
      {"/observation/add", observation_handler, []},
      {"/clear", clear_handler, []}
    ]}
  ]),
  %% Name, NbAcceptors, TransOpts, ProtoOpts
  cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  myapp_sup:start_link().

stop(_State) ->
  ok.
