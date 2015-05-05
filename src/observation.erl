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
      {ValidStart, Missing} = check_start(Start, sequence:path(Sequence), []),
      case ValidStart of
        [] -> {error, no_solutions_found};
        _ ->
          NewSequence = sequence:finish(Start, Sequence),
          {ok, {Start, format_numbers(Missing), NewSequence}}
      end
  end;
process_observation(Sequence, {<<"green">>, Numbers}) ->
  case sequence:finished(Sequence) of
    true -> {error, red_observation_not_last};
    false ->
      ParsedNumbers = parse_numbers(Numbers),
      case guess_start(ParsedNumbers, sequence:start(Sequence), sequence:length(Sequence)) of
        {ok, Start} ->
          {ValidStart, Missing} = check_start(Start, sequence:path(Sequence), [ParsedNumbers]),
          case ValidStart of
            [] -> {error, no_solutions_found};
            _ ->
              NewSequence = sequence:next(ValidStart, ParsedNumbers, Sequence),
              {ok, {Start, format_numbers(Missing), NewSequence}}
          end;
        {error, Err} -> {error, Err}
      end
  end.

-spec check_start([integer()], [numbers()], [numbers()]) -> {[integer()], number()}.
check_start(Start, OldPath, CurrentNumbers) ->
  ObservedPath = CurrentNumbers ++ OldPath,
  Length = erlang:length(ObservedPath),
  PossiblePaths = lists:filtermap(
    fun
      (StartNum) when StartNum < Length; StartNum < 0 -> false;
      (StartNum) ->
        CurrentNum = StartNum + 1 - Length,
        {true, [ integer_to_numbers(N) || N <- lists:seq(CurrentNum, StartNum) ]}
    end,
    Start),
  lists:foldl(
    fun(Path, {Starts, Acc}) -> 
      case compare_path(Path, ObservedPath) of
        {ok, Missing} -> 
          PathStart = numbers_to_integer(lists:last(Path)),
          {[PathStart | Starts], merge_numbers(Missing, Acc)};
        error -> 
          {Starts, Acc}
      end
    end,
    {[], {0,0}},
    PossiblePaths).

-spec compare_path([numbers()], [numbers()]) -> {ok, numbers()} | error.
compare_path(Path, ObsPath) ->
  Zip = lists:zip(Path, ObsPath),
  lists:foldl(
    fun
      ({PathNumber, ObsPathNumber}, {ok, Acc}) ->
        case intersect_numbers(Acc, ObsPathNumber) of
          false ->
            Missing = missing_numbers(PathNumber, ObsPathNumber),
            {ok, merge_numbers(Missing, Acc)};
          true ->
            error
        end;
      (_, error) -> error
    end,
    {ok, {0,0}},
    Zip).

-spec guess_start(numbers(), [integer()], integer()) -> {ok, [integer()]} | {error, no_solutions_found}.
guess_start(Numbers, Start, Length) ->
  OverlapFun = 
    fun(Guess) ->
      NumbersGuess = integer_to_numbers(Guess),
      part_of_numbers(Numbers, NumbersGuess)
    end,
  NewStart = case Start of
    [] -> lists:filter(OverlapFun, lists:seq(1,99));
    _ -> [ StartNum || StartNum <- Start, StartNum > Length, OverlapFun(StartNum - Length) ]
  end,
  case NewStart of
    [] -> {error, no_solutions_found};
    _ -> {ok, NewStart}
  end.

merge_numbers({N1, N2}, {N11, N12}) ->
  {N1 bor N11, N2 bor N12}.

part_of_numbers(Part, Full) ->
  {0,0} == missing_numbers(Part, Full).

intersect_numbers({First1, First2}, {Second1, Second2}) ->
  {0,0} =/= {First1 band Second1, First2 band Second2}.

-spec missing_bits(integer(), integer()) -> integer().
missing_bits(Full, Partial) ->
  Full band bnot Partial.

missing_numbers({Full1, Full2}, {Partial1, Partial2}) ->
  {missing_bits(Full1, Partial1), missing_bits(Full2, Partial2)}.

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
integer_to_numbers(Int) -> {digit_to_number(Int div 10), digit_to_number(Int rem 10)}.

-spec digit_to_number(0..9) -> integer().
digit_to_number(0) -> 119;
digit_to_number(1) -> 18;
digit_to_number(2) -> 93;
digit_to_number(3) -> 91;
digit_to_number(4) -> 58;
digit_to_number(5) -> 107;
digit_to_number(6) -> 111;
digit_to_number(7) -> 82;
digit_to_number(8) -> 127;
digit_to_number(9) -> 123.

-spec number_to_digit(0..9) -> integer().
number_to_digit(119) -> 0;
number_to_digit(18) -> 1;
number_to_digit(93) -> 2;
number_to_digit(91) -> 3;
number_to_digit(58) -> 4;
number_to_digit(107) -> 5;
number_to_digit(111) -> 6;
number_to_digit(82) -> 7;
number_to_digit(127) -> 8;
number_to_digit(123) -> 9.

numbers_to_integer({Tens, Ones}) ->
  number_to_digit(Tens) * 10 + number_to_digit(Ones).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


parse_format_numbers_test_() ->
  lists:map(
    fun(N) ->
      Numbers = integer_to_numbers(N),
      ?_assertEqual(Numbers, parse_numbers(format_numbers(Numbers)))
    end,
    lists:seq(1,99)).

part_of_numbers_test_() ->
  lists:map(
    fun({NumbersBinOver, NumbersBinUnder, Overlap}) ->
      Over = parse_numbers(NumbersBinOver),
      Under = parse_numbers(NumbersBinUnder),
      { iolist_to_binary([ "overlap ", NumbersBinOver, " over ", NumbersBinUnder]),
        ?_assertEqual(Overlap, part_of_numbers(Over, Under))
      }
    end,
    [
      {[<<"0000000">>, <<"0000000">>], [<<"0000000">>, <<"0000000">>], true},
      {[<<"0000000">>, <<"0000000">>], [<<"0000010">>, <<"0001100">>], true},
      {[<<"1111111">>, <<"1111111">>], [<<"1111111">>, <<"1111111">>], true},
      {[<<"1100111">>, <<"1111110">>], [<<"1100111">>, <<"1111110">>], true},
      {[<<"1111111">>, <<"1111111">>], [<<"1111111">>, <<"1111110">>], false},
      {[<<"1111111">>, <<"1111110">>], [<<"1111111">>, <<"1111111">>], true},
      {[<<"1100111">>, <<"1111000">>], [<<"0010010">>, <<"1011001">>], false}
    ]).

guess_start_test_() ->
  lists:map(
    fun({Numbers, Starts, Length, Expected}) ->
      Actual = guess_start(Numbers, Starts, Length),
      { iolist_to_binary(io_lib:format("Guess start on ~p with Starts ~p and sequence length ~p", [Numbers, Starts, Length])),
        ?_assertEqual(Expected, Actual)
      }
    end,
    [
      {parse_numbers([<<"0000000">>, <<"0000000">>]), [], 0, {ok, lists:seq(1,99)}},
      {parse_numbers([<<"0000000">>, <<"0000001">>]), [2, 3, 4], 1, {ok, [3, 4]}},
      {parse_numbers([<<"0001001">>, <<"0101110">>]), lists:seq(1,99), 10, {ok, [36,38,46,48,66,68,76,78,96,98]}},
      {parse_numbers([<<"0001001">>, <<"0101110">>]), lists:seq(1,99), 80, {error, no_solutions_found}},
      {parse_numbers([<<"1111111">>, <<"1111111">>]), lists:seq(1,99), 1, {ok, [89]}},
      {parse_numbers([<<"1111111">>, <<"1111111">>]), [22], 1, {error, no_solutions_found}}
    ]).

compare_path_test_() ->
  lists:map(
    fun({Name, Path, OldPath, Expected}) ->
      
      {
        Name,
        fun() ->
          Actual = case compare_path(Path, OldPath) of
            {ok, Numbers} ->
              {ok, format_numbers(Numbers)};
            _ -> error
          end,
          ?assertEqual(Expected, Actual)
        end
      }
    end,
    [
      {
        "Zero equal zero",
        [parse_numbers([<<"0000000">>, <<"0000000">>])],
        [parse_numbers([<<"0000000">>, <<"0000000">>])],
        {ok, [<<"0000000">>, <<"0000000">>]}
      },
      {
        "Zero path mean all sectors missing",
        [parse_numbers([<<"1111111">>, <<"1111111">>])],
        [parse_numbers([<<"0000000">>, <<"0000000">>])],
        {ok, [<<"1111111">>, <<"1111111">>]}
      },
      {
        "Zero path find only available sectors",
        [parse_numbers([<<"1100000">>, <<"1100000">>])],
        [parse_numbers([<<"0000000">>, <<"0000000">>])],
        {ok, [<<"1100000">>, <<"1100000">>]}
      },
      {
        "Path find available sectors",
        [parse_numbers([<<"1100000">>, <<"1100000">>])],
        [parse_numbers([<<"0001100">>, <<"0010000">>])],
        {ok, [<<"1100000">>, <<"1100000">>]}
      },
      {
        "Sectors in path do not missing",
        [parse_numbers([<<"1100100">>, <<"1100000">>])],
        [parse_numbers([<<"0101100">>, <<"1010000">>])],
        {ok, [<<"1000000">>, <<"0100000">>]}
      },
      {
        "Missing sectors unite in path",
        [parse_numbers([<<"1100100">>, <<"1100000">>]), parse_numbers([<<"0010010">>, <<"1111111">>])],
        [parse_numbers([<<"0101100">>, <<"1000000">>]), parse_numbers([<<"0100100">>, <<"1000000">>])],
        {ok, [<<"1010010">>, <<"0111111">>]}
      },
      {
        "Error if missing in next observation",
        [parse_numbers([<<"1100100">>, <<"1100000">>]), parse_numbers([<<"0010010">>, <<"1111111">>])],
        [parse_numbers([<<"0101100">>, <<"1000000">>]), parse_numbers([<<"0100100">>, <<"1100000">>])],
        error
      }
    ]).

check_start_test_() ->
  lists:map(
    fun({Name,Start, OldPath, CurrentNumbers, Expected}) ->
      
      {Name, 
      fun() ->
        Actual = check_start(Start, OldPath, CurrentNumbers),
        ?assertEqual(Expected, Actual)
      end}
    end,
    [
      {
        "Zero path mean all sectors missing",
        lists:seq(1,99),
        [parse_numbers([<<"0000000">>, <<"0000000">>])],
        [parse_numbers([<<"0000000">>, <<"0000000">>])],
        {lists:reverse(lists:seq(2,99)), parse_numbers([<<"1111111">>, <<"1111111">>])}
      },
      {
        "Empty path fills with current numbers",
        lists:seq(1,99),
        [],
        [parse_numbers([<<"0000000">>, <<"0000000">>])],
        {lists:reverse(lists:seq(1,99)), parse_numbers([<<"1111111">>, <<"1111111">>])}
      },
      {
        "Missing check for path length + 1 numbers",
        [10],
        [parse_numbers([<<"0010000">>, <<"0110111">>]), parse_numbers([<<"0010000">>, <<"0110011">>]), parse_numbers([<<"0010000">>, <<"0110111">>])],
        [parse_numbers([<<"0010000">>, <<"0010010">>])],
        {[10], parse_numbers([<<"1100111">>, <<"1001000">>])}
      },
      {
        "Do not check to short starts",
        [2,3,10],
        [parse_numbers([<<"0010000">>, <<"0110111">>]), parse_numbers([<<"0010000">>, <<"0110011">>]), parse_numbers([<<"0010000">>, <<"0110111">>])],
        [parse_numbers([<<"0010000">>, <<"0010010">>])],
        {[10], parse_numbers([<<"1100111">>, <<"1001000">>])}
      },
      {
        "Remove invalid starts",
        [2,3,10],
        [parse_numbers([<<"1110111">>, <<"0110001">>]), parse_numbers([<<"0000001">>, <<"0110001">>]), parse_numbers([<<"0010000">>, <<"0110111">>])],
        [parse_numbers([<<"1110111">>, <<"1010000">>])],
        {[], {0,0}}
      },
      {
        "Keep valid starts",
        [2,3,10, 92, 29],
        [parse_numbers([<<"0010000">>, <<"0110111">>]), parse_numbers([<<"0010000">>, <<"0110011">>]), parse_numbers([<<"0010000">>, <<"0110111">>])],
        [parse_numbers([<<"0010000">>, <<"0010010">>])],
        {[10], parse_numbers([<<"1100111">>, <<"1001000">>])}
      }
    ]).


-endif.