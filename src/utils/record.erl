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


-module(record).

-export([to_json/1, attributes/1, attributes/2, map_attributes/1, map_attributes/2]).

%%% RecordValue can't be atom
-spec(to_json(tuple()) -> JSONString::binary()).
to_json(Record) ->
    json:encode(attributes(Record)).

-spec(attributes(tuple()) -> [{}]|[{Key::any(), Value::any()}, ...]).
attributes(Record) when Record =:= undefined ->
    [{}];
attributes(Record) ->
    % io:format("Record: ~p~n", [Record]),
    [Name|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Name),
    attributes(Fields, Values, []).

-spec(attributes(Record::tuple(), ExceptKeys::[atom()]) ->
      [{}]|[{Key::any(), Value::any()}, ...]).
attributes(Record, _ExceptKeys) when Record =:= undefined ->
    [{}];
attributes(Record, ExceptKeys) ->
    [Name|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Name),
    Proplist = attributes(Fields, Values, []),
    if
        ExceptKeys =:= [] ->
            Proplist;
        true ->
            {_, Attributes} = proplists:split(Proplist, ExceptKeys),
            Attributes
    end.


-spec(map_attributes([tuple()]) -> []|[[{Key::any(), Value::any()}, ...]]).
map_attributes(Records) when Records =:= [] orelse Records =:= undefined ->
    [];
map_attributes(Records) ->
    map_attributes(Records, [], []).

map_attributes(Records, _ExceptKeys) when Records =:= [] orelse Records =:= undefined ->
    [];
map_attributes(Records, ExceptKeys) ->
    map_attributes(Records, ExceptKeys, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

attributes([], [], Result) ->
    Result;
attributes([Field|TailFields], [Value|TailValues], Result) when Value =:= undefined ->
    attributes(TailFields, TailValues, [{Field, null}|Result]);
attributes([Field|TailFields], [{<<ObjectId:12/binary>>}|TailValues], Result) ->
    BinaryId = db:objectid_to_binary_string({ObjectId}),
    attributes(TailFields, TailValues, [{Field, BinaryId}|Result]);
attributes([Field|TailFields], [{MegaSecs, Secs, _MicroSecs}|TailValues], Result) ->
    UnixTime = MegaSecs * 1000000 + Secs,
    attributes(TailFields, TailValues, [{Field, UnixTime}|Result]);
attributes([Field|TailFields], [Value|TailValues], Result) ->
    attributes(TailFields, TailValues, [{Field, Value}|Result]).

map_attributes([], _ExceptKeys, Result) ->
    Result;
map_attributes([Record|TailRecords], ExceptKeys, Result) when Record =:= undefined ->
    map_attributes(TailRecords, ExceptKeys, Result);
map_attributes([Record|TailRecords], ExceptKeys, Result) ->
    map_attributes(TailRecords, ExceptKeys, [attributes(Record, ExceptKeys)|Result]).
