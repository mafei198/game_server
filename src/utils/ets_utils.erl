%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2024
%% Savin Max <mafei.198@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.


%%% @doc
%%%        Some easy to use functions for operate ets.
%%% @end
%%% Created :  一 10 07 00:01:33 2013 by Savin-Max µ

-module(ets_utils).

-export([makepat/1,
         make_element_spec/1,
         migrate/2,
         each/2
        ]).

%%--------------------------------------------------------------------
%% @doc:    Construct {'_', Value1, ..., '_', ValueN} for ets:match_object(Tab, Pattern)
%% @spec:    makepat(SelectorRecord::record() ) -> tuple().
%% @end
%%--------------------------------------------------------------------

-spec(makepat(tuple()) -> tuple() ).
makepat(Record) ->
    makepat(tuple_to_list(Record), []).

makepat([], Result) ->
    list_to_tuple(Result);
makepat([Value|ValueList], Result) when Value =:= undefined ->
    makepat(ValueList, Result ++ ['_']);
makepat([Value|ValueList], Result) ->
    makepat(ValueList, Result ++ [Value]).

%%--------------------------------------------------------------------
%% @doc:    Construct [{Pos, Value}, ...] for ets:update_element(Tab, Key, [{Pos, Value}])
%% @spec:    make_element_spec(ModifierRecord::record( ) -> [tuple(), ...].
%% @end
%%--------------------------------------------------------------------

-spec(make_element_spec(tuple()) -> [tuple(), ...]).
make_element_spec(ModifierRecord) ->
    [_Name|ValueList] = tuple_to_list(ModifierRecord),
    make_element_spec(ValueList, 2, []).

make_element_spec([], _Position, Result) ->
    Result;
make_element_spec([Value|ValueList], Position, Result) when Value =:= undefined ->
    make_element_spec(ValueList, Position + 1, Result);
make_element_spec([Value|ValueList], Position, Result) ->
    make_element_spec(ValueList, Position + 1, Result ++ [{Position, Value}]).

migrate(Table, TransformFun) ->
    each(Table, fun(OldRec) ->
        NewRec = TransformFun(OldRec),
        if
            NewRec =:= OldRec -> {break, already_migrated_break};
            true -> ets:insert(NewRec)
        end
    end),
    migrate_success.

each(Table, Fun) ->
    each(Table, ets:first(Table), Fun).

each(_Table, '$end_of_table', _Fun) -> ok;
each(Table, Key, Fun) ->
    [Value] = ets:lookup(Table, Key),
    case Fun(Value) of
        {break, V} -> V;
        true -> each(Table, ets:next(Key), Fun)
    end.
