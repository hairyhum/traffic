-module(sequence).

-export([
  new/1,
  id/1,
  length/1, 
  path/1, 
  start/1, 
  finish/2, 
  next/3, 
  finished/1
]).


-record(sequence,{
  id,
  start,
  path,
  finished
  }).

-type sequence() :: #sequence{}.
-type numbers() :: {integer(), integer()}.
-type sequence_id() :: binary().

-export_type([sequence/0, sequence_id/0]).

-spec new(sequence_id()) -> sequence().
new(SequenceId) ->
  #sequence{
    id = SequenceId
  }.

-spec id(sequence()) -> sequence_id() | undefined.
id(#sequence{ id = Id }) -> Id.

-spec length(sequence()) -> integer().
length(Sequence) -> erlang:length(path(Sequence)).

-spec path(sequence()) -> [numbers()].
path(#sequence{ path = undefined }) -> [];
path(#sequence{ path = Path }) -> Path.

-spec start(sequence()) -> [integer()].
start(#sequence{ start = undefined }) -> [];
start(#sequence{ start = Start }) -> Start.

-spec finish([integer()], sequence()) -> sequence().
finish(Start, Sequence) ->
  Sequence#sequence{ finished = true, start = Start }.

-spec finished(sequence()) -> boolean().
finished(Sequence) ->
  proplists:get_value(<<"finished">>, Sequence, false).

-spec next([integer()], numbers(), sequence()) -> sequence().
next(Start, Numbers, Sequence) ->
  Path = sequence:path(Sequence),
  Sequence#sequence{
    path = [Numbers | Path],
    start = Start
  }.