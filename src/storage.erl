-module(storage).

-export([start/0]).
-export([find/1, new/1, update/2, clear/0]).

start() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(sequence, []).

clear() ->
  mnesia:clear_table(sequence),
  ok.

new(SequenceId) ->
  mnesia:dirty_write(sequence, {sequence, SequenceId, []}).

find(SequenceId) ->
  case mnesia:dirty_read(sequence, SequenceId) of
    [] -> {error, not_found};
    [{sequence, SequenceId, Data}] -> {ok, Data}
  end.

update(SequenceId, Data) ->
  mnesia:dirty_write(sequence, {sequence, SequenceId, Data}).
