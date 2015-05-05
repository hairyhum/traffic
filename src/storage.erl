-module(storage).

-export([start/0]).
-export([find/1, save/1, clear/0]).

start() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(sequence, [{attributes, sequence:schema()}]).

clear() ->
  mnesia:clear_table(sequence),
  ok.

save(Sequence) ->
  Id = sequence:id(Sequence),
  case find(Id) of
    {error, not_found} -> create(Sequence);
    {ok, _} -> update(Sequence)
  end. 

create(Sequence) ->
  mnesia:dirty_write(sequence, Sequence).

find(SequenceId) ->
  case mnesia:dirty_read(sequence, SequenceId) of
    [] -> {error, not_found};
    [Sequence] -> {ok, Sequence}
  end.

update(Sequence) ->
  mnesia:dirty_write(sequence, Sequence).
