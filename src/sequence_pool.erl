-module(sequence_pool).
-behaviour(supervisor).

-export([start_child/1, get_child/2]).
-export([start_link/0, init/1]).

start_link() ->
      supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 10000, 1}, []}}.

child_spec(SequenceId, SequenceData) ->
  {SequenceId, {sequence_worker, start_link, [SequenceData]},
    permanent, 5000, worker, [sequence_worker]}.

start_child(SequenceId) ->
  get_child(SequenceId, []).

get_child(SequenceId, SequenceData) ->  
  Spec = child_spec(SequenceId, SequenceData),
  case supervisor:start_child(?MODULE, Spec) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid};
    {error, Reason} -> {error, Reason}
  end.


