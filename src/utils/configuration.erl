-module(configuration).

-export([init/2,
         get/2,
         get/3,
         all/1,
         update/3]).

init(NameSpace, Value) ->
    mochiglobal:put(NameSpace, Value).

get(NameSpace, Field) ->
    get(NameSpace, Field, undefined).

get(NameSpace, Field, Default) ->
    Result = case mochiglobal:get(NameSpace) of
        undefined -> Default;
        List -> 
            case proplists:get_value(Field, List) of
                undefined -> Default;
                Value -> Value
            end
    end,
    % error_logger:info_msg("NameSpace: ~p, Field: ~p, Default: ~p, Result: ~p~n", 
    %                       [NameSpace, Field, Default, Result]),
    Result.

all(NameSpace) ->
    mochiglobal:get(NameSpace).

update(NameSpace, Field, Value) ->
    case mochiglobal:get(NameSpace) of
        undefined -> undefined;
        List -> 
            NList = lists:keyreplace(Field, 1, List, {Field, Value}),
            init(NameSpace, NList)
    end.
