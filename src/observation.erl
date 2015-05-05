-module(observation).

-export([
  new_sequence/0, 
  check_observation/2, 
  clear/0]).

-type numbers() :: {integer(), integer()}.
-type sector_code() :: binary().
-type observation_error() :: sequence_not_found | no_solutions_found | not_enough_data | red_observation_not_last.

-spec new_sequence() -> {ok, sequence:sequence_id()}.
new_sequence() ->
  SequenceId = generate_id(4), 
  Sequence = sequence:new(SequenceId),
  storage:save(Sequence),
  {ok, SequenceId}.

-spec check_observation(sequence:sequence_id(), {binary(), [sector_code()]}) -> 
    {ok, {[integer()], [sector_code()]}} | {error, observation_error()}.
check_observation(SequenceId, Observation) ->
lager:error("Observation ~p", [Observation]),
  case storage:find(SequenceId) of
    {ok, Sequence} -> 
      case process_observation(Sequence, Observation) of
        {ok, {Start, Missing, NewSequence}} -> 
          storage:save(NewSequence),
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
      lager:error("calculated missing ~p", [Missing]),
      NewSequence = sequence:finish(Start, Sequence),
      {ok, {Start, Missing, NewSequence}}
  end;
process_observation(Sequence, {<<"green">>, Numbers}) ->
  case sequence:finished(Sequence) of
    true -> {error, red_observation_not_last};
    false ->
      ParsedNumbers = parse_numbers(Numbers),
      case guess_start(ParsedNumbers, Sequence) of
        {ok, Start} ->
          lager:error("starts ~p", [Start]),
          Missing = calc_missing(Start, Sequence, [ParsedNumbers]),
          lager:error("calculated missing ~p", [Missing]),
          NewSequence = sequence:next(Start, ParsedNumbers, Sequence),
          {ok, {Start, Missing, NewSequence}};
        {error, Err} -> {error, Err}
      end
  end.

-spec calc_missing([integer()], sequence:sequence(), [numbers()]) -> [sector_code()].
calc_missing(Start, Sequence, CurrentNumbers) ->
  NumbersPath = CurrentNumbers ++ sequence:path(Sequence),
  Length = erlang:length(NumbersPath),
  PossiblePaths = lists:filtermap(
    fun
      (StartNum) when StartNum >= Length; StartNum > 0 -> false;
      (StartNum) ->
        CurrentNum = StartNum + 1 - Length,
        [ integer_to_numbers(N) || N <- lists:seq(CurrentNum, StartNum) ]
    end,
    Start),
  Missing = fold_numbers(
    fun(Path) ->
      compare_path(Path, NumbersPath)
    end,
    PossiblePaths),
  format_numbers(Missing).

-spec compare_path([numbers()], [numbers()]) -> numbers().
compare_path(Path, NumbersPath) ->
  Merged = lists:zipwith(
    fun({PathN1, PathN2}, {NPathN1, NPathN2}) ->
      Missing1 = missing_bits(PathN1, NPathN1),
      Missing2 = missing_bits(PathN2, NPathN2),
      {Missing1, Missing2}
    end, 
    Path, 
    NumbersPath),
  fold_numbers(fun(X) -> X end, Merged).

-spec fold_numbers(fun((X) -> numbers()), [X]) -> numbers().
fold_numbers(Fun, List) ->
  lists:foldl(
    fun(X, {Acc1, Acc2}) ->
      {Num1, Num2} = Fun(X),
      {Num1 bor Acc1, Num2 bor Acc2}
    end,
    {0,0},
    List).

-spec guess_start(numbers(), sequence:sequence()) -> {ok, [integer()]} | {error, observation_error}.
guess_start(Numbers, Sequence) ->
  NumberGuess = lists:filter(
    fun(Guess) ->
      NumbersGuess = integer_to_numbers(Guess),
      overlaps(Numbers, NumbersGuess)
    end,
    lsits:seq(1, 99)),
  case sequence:start(Sequence) of
    [] -> {ok, NumberGuess};
    Start -> {ok, [ Guess || Guess <- Start, lists:member(Guess - 1, NumberGuess) ]}
  end.

overlaps({Over1, Over2}, {Under1, Under2}) ->
  missing_bits(Over1, Under1) == 0
  andalso
  missing_bits(Over2, Under2) == 0.

-spec missing_bits(integer(), integer()) -> integer().
missing_bits(Full, Partial) ->
  Full band bnot Partial.

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

-spec parse_numbers([binary()]) -> numbers().
parse_numbers([Tens, Ones]) -> 
  { binary_to_integer(Tens, 2), binary_to_integer(Ones, 2) }.

-spec format_numbers(numbers()) -> sector_code().
format_numbers({N1, N2}) ->
  [number_to_binary(N1), number_to_binary(N2)].

-spec number_to_binary(integer()) -> sector_code().
number_to_binary(Missing) ->
  Bin = integer_to_binary(Missing, 2),
  Filler = binary:copy(<<"0">>, 7 - byte_size(Bin)),
  <<Filler/binary, Bin/binary>>.

-spec integer_to_numbers(0..99) -> numbers().
integer_to_numbers(Int) -> {encode_number(Int div 10), encode_number(Int rem 10)}.

-spec encode_number(0..9) -> integer().
encode_number(0) -> 119;
encode_number(1) -> 18;
encode_number(2) -> 93;
encode_number(3) -> 91;
encode_number(4) -> 58;
encode_number(5) -> 107;
encode_number(6) -> 111;
encode_number(7) -> 82;
encode_number(8) -> 127;
encode_number(9) -> 123.