-module(middleware).

-export([inject/3, handle/2]).

inject(Key, Module, Function) ->
    case mochiglobal:get(Key) of
        undefined -> mochiglobal:put(Key, {Module, Function});
        Value -> error("middleware registe [~p] conflict. Old value: ~p~n", [Key, Value])
    end.

handle(Key, Args) ->
    case mochiglobal:get(Key) of
        undefined -> 
            {fail, handler_not_found};
        {Module, Function} -> 
            {ok, apply(Module, Function, Args)}
    end.
