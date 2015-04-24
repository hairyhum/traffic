-module(sequence).

-export([new_sequence/0, check_observation/2, clear/0]).
-export([length/1, path/1, start/1, finish/3, next/4, finished/1]).


-type sequence_id() :: binary().
-type sector_code() :: binary().
-type observation_error() :: sequence_not_found | no_solutions_found | not_enough_data | red_observation_not_last.
-type sequence() :: list().

-spec new_sequence() -> {ok, sequence_id()}.
new_sequence() ->
  SequenceId = generate_id(4), 
  storage:new(SequenceId),
  {ok, SequenceId}.

-spec check_observation(sequence_id(), {binary(), [sector_code()]}) -> 
    {ok, {[integer()], [sector_code()]}} | {error, observation_error()}.
check_observation(SequenceId, Observation) ->
  case get_sequence(SequenceId) of
    {ok, Sequence} -> 
      case process_observation(Sequence, Observation) of
        {ok, {Start, Missing, NewSequence}} -> 
          storage:update(SequenceId, NewSequence),
          {ok, {Start, Missing}};
        {error, Err} -> {error, Err}
      end;
    {error, not_found} -> {error, sequence_not_found}
  end.

-spec process_observation(list(), {binary(), [sector_code()]}) ->
    {ok, {[integer()], [sector_code()], list()}} | {error, observation_error()}.
process_observation(Sequence, {<<"red">>, _}) ->
  case sequence:length(Sequence) of
    0 -> {error, not_enough_data};
    N ->
      Start = [N],
      Missing = calc_missing(Start, Sequence, []),
      NewSequence = sequence:finish(Start, Missing, Sequence),
      {ok, {Start, Missing, NewSequence}}
  end;
process_observation(Sequence, {<<"green">>, Numbers}) ->
  case sequence:finished(Sequence) of
    true -> {error, red_observation_not_last};
    false ->
      EncodedNumbers = encode_numbers(Numbers),
      case guess_start(EncodedNumbers, Sequence) of
        {ok, Start} ->
          Missing = calc_missing(Start, Sequence, [EncodedNumbers]),
          NewSequence = sequence:next(Start, Missing, EncodedNumbers, Sequence),
          {ok, {Start, Missing, NewSequence}};
        {error, Err} -> {error, Err}
      end
  end.

-spec calc_missing([integer()], sequence(), [number()]) -> [sector_code()].
calc_missing(Start, Sequence, CurrentNumbers) ->
  Length = sequence:length(Sequence),
  Paths = [ lists:seq(StartNum - Length, StartNum) || StartNum <- Start ],
  NumbersPath = CurrentNumbers ++ sequence:path(Sequence),
  Missing = lists:filtermap(
    fun(Path) ->
      EncodedPath = [ {encode_integer(N div 10), encode_integer(N rem 10)} || N <- Path ],
      case compare_path(EncodedPath, NumbersPath) of
        {ok, Missing} -> {true, Missing};
        error -> false
      end
    end,
    Paths),
  decode_numbers(lists:flatten(Missing)).

-spec decode_numbers([integer()]) -> [sector_code()].
decode_numbers(Missing) ->
  lists:map(
    fun(Num) ->
      Bin = integer_to_binary(Num, 2),
      Filler = binary:copy(<<"0">>, 7 - byte_size(Bin)),
      <<Filler/binary, Bin/binary>>
    end,
    Missing).

-spec encode_numbers([sector_code()]) -> {integer(), integer()}.
encode_numbers([N1, N2]) -> { binary_to_integer(N1, 2), binary_to_integer(N2, 2) }.

-spec encode_integer(integer()) -> integer().
encode_integer(0) -> 119;
encode_integer(1) -> 18;
encode_integer(2) -> 93;
encode_integer(3) -> 91;
encode_integer(4) -> 58;
encode_integer(5) -> 107;
encode_integer(6) -> 111;
encode_integer(7) -> 82;
encode_integer(8) -> 127;
encode_integer(9) -> 123.

-spec compare_path([number()], [number()]) -> {ok, [integer()]} | error.
compare_path(Path, NumbersPath) ->
  Merged = lists:zipwith(
    fun({PathN1, PathN2}, {NPathN1, NPathN2}) ->
      Missing1 = PathN1 band bnot NPathN1,
      Missing2 = PathN2 band bnot NPathN2,
      {Missing1, Missing2}
    end, 
    Path, 
    NumbersPath),
  lists:foldl(
    fun
      (_, false) -> error;
      ({0, _}, _) -> error;
      ({_, 0}, _) -> error;
      ({M1, M2}, {ok, Missing}) -> {ok, [M1, M2 | Missing]}
    end,
    {ok, []},
    Merged).

-spec guess_start({integer(), integer()}, sequence()) -> {ok, [integer()]} | {error, observation_error}.
guess_start({Number1, Number2}, Sequence) ->
  NumberGuess = [ N1 * 10 + N2 
                 || N1 <- lists:seq(0, 9), N2 <- lists:seq(0, 9),
                 overlaps(Number1, N1),
                 overlaps(Number2, N2) ],
  case sequence:start(Sequence) of
    [] -> {ok, NumberGuess};
    Start -> {ok, [ Guess || Guess <- Start, lists:member(Guess - 1, NumberGuess) ]}
  end.

overlaps(Overlaper, Overlapee) ->
  Overlaper band bnot encode_integer(Overlapee) == 0.

-spec length(sequence()) -> integer().
length(Sequence) -> erlang:length(path(Sequence)).

-type numbers() :: {integer(), integer()}.
-spec path(sequence()) -> [numbers()].
path(Sequence) ->
  proplists:get_value(<<"path">>, Sequence, []).

-spec start(sequence()) -> [integer()].
start(Sequence) ->
  proplists:get_value(<<"start">>, Sequence, []).

-spec finish([integer()], [integer()], sequence()) -> sequence().
finish(Start, Missing, Sequence) ->
  lists:ukeymerge(1, [{<<"finished">>, true}, {<<"start">>, Start}], Sequence).

-spec finished(sequence()) -> boolean().
finished(Sequence) ->
  proplists:get_value(<<"finished">>, Sequence, false).

-spec next([integer()], [integer()], numbers(), sequence()) -> sequence().
next(Start, Missing, Numbers, Sequence) ->
  Path = sequence:path(Sequence),
  lists:ukeymerge(1, [{<<"path">>, [Numbers | Path]}, {<<"start">>, Start}], Sequence).



-spec get_sequence(sequence_id()) -> {ok, list()} | {error, not_found}.
get_sequence(SequenceId) ->
  case storage:find(SequenceId) of
    {ok, SequenceData} -> {ok, SequenceData};
    {error, not_found} -> {error, not_found}
  end.

-spec clear() -> ok.
clear() ->
  storage:clear().

generate_id(0) ->
  error(unable_to_generate_id);
generate_id(Tries) ->
  Id = uuid:generate(),
  case storage:find(Id) of
    {error, not_found} -> Id;
    {ok, _} -> generate_id(Tries - 1)
  end.
