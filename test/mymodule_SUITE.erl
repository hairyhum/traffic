%% common_test suite for mymodule

-module(mymodule_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}}].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%
%%      NB: By default, we export all 1-arity user defined functions
%%--------------------------------------------------------------------
all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({test_create_sequence,1}) -> true;
                                   ({test_add_observation,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    inets:start(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
    Config.

test_mymodule() ->
    [{userdata,[{doc,"Testing the mymodule module"}]}].

test_mymodule(_Config) ->
    {skip,"Not implemented."}.

test_create_sequence() ->
  [{userdata, [{doc, "Create sequence generate new UUID"}]}].
test_create_sequence(_Config) ->
  {200, Data} = http_req("/sequence/create", post),
  {200, Data1} = http_req("/sequence/create", post),
  Json = jsonx:decode(Data, [{format, proplist}]),
  <<"ok">> = proplists:get_value(<<"status">>, Json),
  UUID = proplists:get_value(<<"sequence">>, Json),
  Json1 = jsonx:decode(Data1, [{format, proplist}]),
  <<"ok">> = proplists:get_value(<<"status">>, Json1),
  UUID1 = proplists:get_value(<<"sequence">>, Json1),
  true = is_uuid(UUID),
  true = is_uuid(UUID1),
  true = Data1 =/= Data.


test_add_observation() ->
  [
    {userdata, [{doc, "Add observation to sequence"}]},
    {require, examples},
    {default_config, examples, [
      {2, ["0001000","0001010"]},
      {15, ["0001000","0101000"]},
      {43, ["1100011","0101101"]}
    ]}
  ].
test_add_observation(Config) ->
  Examples = ct:get_config(examples),
  lists:map(
    fun({Start, Missing}) ->
      {200, SeqRes} = http_req("/sequence/create", post),
      SeqNum = proplists:get_value(<<"sequence">>, jsonx:decode(SeqRes, [{format, proplist}])),
      Output = lists:foldl(
        fun(ObsNumber, Acc) ->
          Data = gen_observation(ObsNumber, Missing, SeqNum),
          {200, ObsData} = http_req("/observation/add", post, Data),
          [ ObsData | Acc ]
        end,
        [],
        lists:reverse(lists:seq(0, Start))),
      [ Last | _Rest ] = Output,
      {StartResults, MissingResults} = parse_observation_result(Last),
      io:format("Start ~p Starts ~p", [Start, StartResults]),
      true = lists:member(Start, StartResults),
      numbers_overlap(Missing, MissingResults)
    end,
    Examples).

numbers_overlap(Missing, MissingResults) ->
  [LM1, LM2] = Missing,
  [LMR1, LMR2] = [ binary_to_list(MR) || MR <- MissingResults ],
  OverlapFun = fun($1, $0) -> $1; (_, _) -> $0 end,
  "0000000" = lists:zipwith(OverlapFun, LMR1, LM1),
  "0000000" = lists:zipwith(OverlapFun, LMR2, LM2).
  
gen_observation(0, _, SeqNum) ->
  [ {sequence, SeqNum}, {observation, [{color, red}]} ];
gen_observation(ObsNumber, Missing, SeqNum) ->
  [LM1, LM2] = Missing,
  {LN1, LN2} = integer_to_numbers(ObsNumber),
  RemoveMissingFun = fun
    (_, $1) -> $0;
    (N, $0) -> N
  end,
  N1 = lists:zipwith(RemoveMissingFun, LN1, LM1),
  N2 = lists:zipwith(RemoveMissingFun, LN2, LM2),
  [
    {sequence, SeqNum},
    {observation, [{color, green}, {numbers, [list_to_binary(N1), list_to_binary(N2)]}]}
  ].

parse_observation_result(Data) ->
  Json = jsonx:decode(Data, [{format, proplist}]),
  <<"ok">> = proplists:get_value(<<"status">>, Json),
  Response = proplists:get_value(<<"response">>, Json),
  true = is_list(Response),
  io:format("Response ~p", [Response]),
  Start = proplists:get_value(<<"start">>, Response),
  Missing = proplists:get_value(<<"missing">>, Response),
  {Start, Missing}.

integer_to_numbers(Int) ->
  Int1 = Int div 10,
  Int2 = Int rem 10,
  { integer_to_number(Int1), integer_to_number(Int2) }.

integer_to_number(0) -> "1110111";
integer_to_number(1) -> "0010010";
integer_to_number(2) -> "1011101";
integer_to_number(3) -> "1011011";
integer_to_number(4) -> "0111010";
integer_to_number(5) -> "1101011";
integer_to_number(6) -> "1101111";
integer_to_number(7) -> "1010010";
integer_to_number(8) -> "1111111";
integer_to_number(9) -> "1111011".


is_uuid(Bin) when not is_binary(Bin) -> false;
is_uuid(Bin) ->
  <<"000000000000000000000000000000000000">> ==
  << 
    <<(case lists:member(B, "1234567890abcdef-") of 
        true -> $0; 
        false -> B 
      end)>> 
    || <<B>> <= Bin >>.


http_req(Url, Method) -> http_req(Url, Method, []).
http_req(Url, Method, Data) ->
  FullUrl = "http://localhost:8080" ++ Url,
  ReqBody = jsonx:encode(Data),
  io:format("Req data ~p", [ReqBody]),
  {ok, {{_, Status, _}, _, Result}} = httpc:request(Method, {FullUrl, [], "application/json", ReqBody}, [], [{body_format, binary}]),
  {Status, Result}.


