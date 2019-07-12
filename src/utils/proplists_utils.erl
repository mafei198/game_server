-module(proplists_utils).
-export([values/1]).

values(List) ->
    values(List, []).

values([], []) -> {};
values([], Result) -> list_to_tuple(lists:reverse(Result));
values([{_Key, Value}|List], Result) ->
    values(List, [Value|Result]).
