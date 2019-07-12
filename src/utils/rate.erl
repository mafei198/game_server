-module(rate).

-export([happen/1, 
         choose/1, 
         choose_n_uniq/2,
         range/1, 
         collect/2, 
         select_n/2, %% return N unique items
         collect_n/2, %% return N items, same item may appear than once
         collect_one/1,
         rand_list/1,
         rand_list_n/2]).

-on_load(init/0).

init() ->
    % mtwist:seed(time_utils:now()),
    rand:seed(exs1024, erlang:timestamp()),
    ok.

happen(Rate) when Rate == 1.0 -> true;
happen(Rate) when Rate < 1 ->
    happen(Rate * 10000);
happen(Rate) ->
    % RandomValue = mtwist:uniform(10000),
    RandomValue = rand:uniform(10000),
    compare(RandomValue, Rate).

%% Rates = [{RateA, ValueA}, {RateB, ValueB}, {default, DefaultValue}],
choose([{_Rate, Value}]) -> Value;
choose(Rates) ->
    {_Rate, Value} = do_choose(Rates),
    Value.

do_choose(Rates) ->
    MaxValue = lists:foldl(fun({Rate, _}, Result) ->
                            Result + Rate
                        end, 0, Rates),
    RandomValue = rand:uniform(MaxValue),
    {Rate, Value} = choose(Rates, RandomValue, 0),
    {Rate, Value}.

choose([{Rate, Value}|Rates], RandomValue, Offset) ->
    case RandomValue =< Rate + Offset of
        true -> {Rate, Value};
        false -> choose(Rates, RandomValue, Offset + Rate)
    end.

choose_n_uniq(Rates, N) ->
    choose_n_uniq(Rates, N, []).

choose_n_uniq(_Rates, 0, Result) -> Result;
choose_n_uniq(Rates, N, Result) ->
    {Rate, Value} = do_choose(Rates),
    NRates = lists:delete({Rate, Value}, Rates),
    choose_n_uniq(NRates, N - 1, [Value|Result]).

%% Range = [{RateA, ValueA}, {RateB, ValueB}, {default, DefaultValue}],
%% select the happened Rate 
range(Range) ->
    % select(mtwist:uniform(10000), Range).
    select(rand:uniform(10000), Range).

select(RandomValue, [{Rate, Value}|_Range]) when RandomValue =< Rate -> Value;
select(_RandomValue, [{default, Value}]) -> Value;
select(RandomValue, [{_Rate, _Value}|Range]) ->
    select(RandomValue, Range).

compare(A, B) when A < B -> true;
compare(_A, _B) -> false.


%% Rand return N elements from Rates
%% Rates = [{RateA, ValueA}, {RateB, ValueB}, {RateC, ValueC}],
%% N =< length(Rates)
collect(Rates, N) ->
    MaxValue = lists:foldl(fun({Rate, _}, Result) ->
                               Result + Rate
                           end, 0, Rates),
    collect(N, Rates, MaxValue, []).

collect(0, _Rates, _MaxValue, Result) -> Result;
collect(N, Rates, MaxValue, Result) ->
    % RandValue = mtwist:uniform(MaxValue),
    RandValue = rand:uniform(MaxValue),
    {Rate, Value} = choose(Rates, RandValue, 0),
    NewRates = lists:delete({Rate, Value}, Rates),
    collect(N - 1, NewRates, MaxValue - Rate, [Value|Result]).

%% Select N elements from List
select_n(List, N) -> select_n(List, N, []).

select_n([], _N, Result) -> Result;
select_n(_List, 0, Result) -> Result;
select_n(List, N, Result) ->
    Len = length(List),
    % Index = mtwist:uniform(Len) + 1,
    Index = rand:uniform(Len),
    Elem = lists:nth(Index, List),
    select_n(lists:delete(Elem, List), N - 1, [Elem|Result]).

collect_n(List, N) -> collect_n(List, N, []).

collect_n(_List, 0, Result) -> Result;
collect_n(List, N, Result) ->
    Len = length(List),
    % Index = trunc(mtwist:uniform(Len)) + 1,
    Index = rand:uniform(Len),
    Elem = lists:nth(Index, List),
    collect_n(List, N - 1, [Elem|Result]).

collect_one(List) ->
    [Elem] = collect_n(List, 1),
    Elem.

rand_list(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

rand_list_n(List, Limit) ->
    SortedList = lists:sort([{rand:uniform(), N} || N <- List]),
    {Result, _} = misc_utils:each(fun({_Fator, Elem}, {Acc, Idx}) ->
        if
            Idx =:= Limit ->
                {break, {Acc, Idx}};
            Idx < Limit ->
                {[Elem|Acc], Idx + 1}
        end
    end, {[], 0}, SortedList),
    Result.
