-module(traffic_light_app).

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
  application:start(traffic_light).


start(_StartType, _StartArgs) ->
  storage:start(),
  {ok, Port} = application:get_env(port),
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
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  traffic_light_sup:start_link().

stop(_State) ->
  ok.
