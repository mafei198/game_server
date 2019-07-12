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


-module (utils_protocol).
-export ([encode/1,
          decode/2,
          encode_boolean/1,
          decode_boolean/1,
          encode_char/1,
          decode_char/1,
          encode_short/1,
          decode_short/1,
          encode_integer/1,
          decode_integer/1,
          encode_float/1,
          decode_float/1,
          encode_array/2,
          decode_array/2,
          encode_tuple/1,
          decode_tuple/2,
          encode_list/1,
          decode_list/2,
          encode_string/1,
          decode_string/1,
          encode_text/1,
          decode_text/1]).

-include("include/protocol.hrl").

%%单字节整数
encode_char(Char) when Char =:= true ->
    <<1:?CHAR>>;
encode_char(Char) when Char =:= false ->
    <<0:?CHAR>>;
encode_char(Char) when Char =:= undefined ->
    <<0:?CHAR>>;
encode_char(Char) when is_integer(Char) ->
    <<Char:?CHAR>>.
decode_char(<<Char:?CHAR, Data/binary>>) ->
    {Char, Data}.

encode_boolean(Boolean) ->
    if
        Boolean =:= true orelse Boolean =:= 1 ->
            <<1:?BOOLEAN>>;
        Boolean =:= false orelse Boolean =:= 0 ->
            <<0:?BOOLEAN>>
    end.
decode_boolean(<<BOOLEAN:?BOOLEAN, Data/binary>>) ->
    if
        BOOLEAN =:= 1 -> {true, Data};
        true -> {false, Data}
    end.

%%短整数
encode_short(Short) when Short =:= undefined ->
    <<0:?SHORT>>;
encode_short(Short) when is_integer(Short) ->
    <<Short:?SHORT>>;
encode_short(Short) when is_float(Short) ->
    Sh = trunc(Short),
    <<Sh:?SHORT>>.
decode_short(<<Short:?SHORT/signed, Data/binary>>) ->
    {Short, Data}.

%%整数
encode_integer(Integer) when Integer =:= undefined ->
    <<0:?INTEGER>>;
encode_integer(Integer) when is_integer(Integer) ->
    <<Integer:?INTEGER>>;
encode_integer(Integer) when is_float(Integer) ->
    Int = trunc(Integer),
    <<Int:?INTEGER>>.
decode_integer(<<Integer:?INTEGER/signed, Data/binary>>) ->
    {Integer, Data}.

%%浮点数
encode_float(Float) ->
    <<Float:?FLOAT/float>>.
decode_float(<<Float:?FLOAT/float, Data/binary>>) ->
    {Float, Data}.

%%字符串
encode_string(undefined) ->
    String = <<"">>,
    Length = byte_size(String),
    list_to_binary([<<Length:?STRING>>, String]);
encode_string(String) when is_binary(String) ->
    Length = byte_size(String),
    list_to_binary([<<Length:?STRING>>, String]);
encode_string(List) when is_list(List) ->
    String = list_to_binary(List),
    Length = byte_size(String),
    list_to_binary([<<Length:?STRING>>, String]);
encode_string(Atom) when is_atom(Atom) ->
    String = atom_to_binary(Atom, utf8),
    Length = byte_size(String),
    list_to_binary([<<Length:?STRING>>, String]);
encode_string(Integer) when is_integer(Integer) ->
    String = integer_to_binary(Integer),
    Length = byte_size(String),
    list_to_binary([<<Length:?STRING>>, String]).
decode_string(<<Length:?STRING/unsigned-big-integer, Data/binary>>) ->
    {StringData, StringLeftData} = split_binary(Data, Length),
    {StringData, StringLeftData}.

encode_text(String) ->
    Length = byte_size(String),
    list_to_binary([<<Length:?TEXT>>, String]).

decode_text(<<Length:?TEXT/unsigned-big-integer, Data/binary>>) ->
    {StringData, StringLeftData} = split_binary(Data, Length),
    {StringData, StringLeftData}.

encode_array(Array, Fun) when is_list(Array) ->
    Len = length(Array),
    DataList = [Fun(Item) || Item <- Array],
    list_to_binary([<<Len:?ARRAY/integer>>, DataList]).
decode_array(<<ArrayLen:?ARRAY, Data/binary>>, Fun) ->
    {Array, LeftData} = decode_array(ArrayLen, Data, Fun, []),
    {Array, LeftData}.

decode_array(0, DataLeft, _Fun, Result) ->
    {lists:reverse(Result), DataLeft};
decode_array(ArrayLen, Data, Fun, Result) ->
    {Item, Bin} = Fun(Data),
    decode_array(ArrayLen - 1, Bin, Fun, [Item|Result]).

%% 编码元组
encode_tuple(Tuple) when is_tuple(Tuple) ->
    DataList = [encode(Item) || Item <- tuple_to_list(Tuple)],
    list_to_binary(DataList).

%% 解码元组
%% 例子: {1, 1.0, <<"hello">>, ...}  解码规则: {integer, float, string, ...}
%% 例子: {1, 1.0, <<"hello">>, [{1, 1.0, <<"world">>, ...}, ...]}  解码规则: {integer, float, string, [{integer, float, string, ...}], ...}
decode_tuple(<<Data/binary>>, DecodeRule) ->
    DecodedList = decode(Data, tuple_to_list(DecodeRule)),
    TupleSize = tuple_size(DecodeRule),
    {Array, [DataLeft]} = lists:split(TupleSize, DecodedList),
    {list_to_tuple(Array), DataLeft}.

%% 编码列表
encode_list(List) when is_list(List) ->
    Len = length(List),
    DataList = [encode(Item) || Item <- List],
    list_to_binary([<<Len:?ARRAY/integer>>, DataList]).

%% 解码列表
%% 规则: 只能解码规则数组，即数组内每个元素的类型和结构必须一样
%% =====================基础解码例子==========================
%% 例子: [1, 2, 3, ...]  解码规则: [integer]
%% 例子: [1.0, 2.0, 3.0, ...]  解码规则: [float]
%% 例子: [<<"a">>, <<"b">>, <<"c">>, ...]  解码规则: [string]
%% 例子: [{1, 2.0, <<"hello">>, ...}, {2, 3.0, <<"world">>, ...}, ...]  解码规则: [{integer, float, string, ...}]
%% =====================嵌套解码例子==========================
%% 例子: [[1, 2, 3, ...], ...]  解码规则: [[integer]]
%% 例子: [[1.0, 2.0, 3.0, ...], ...]  解码规则: [[float]]
%% 例子: [[<<"a">>, <<"b">>, <<"c">>, ...], ...]  解码规则: [[string]]
%% 例子: [[{1, 2.0, <<"hello">>, ...}, {2, 3.0, <<"world">>, ...}, ...], ...]  解码规则: [[{integer, float, string, ...}]]
%% 例子: ...
decode_list(<<ListLen:?ARRAY, Data/binary>>, [RepeatElement | _]) ->
    TypeList = lists:duplicate(ListLen, RepeatElement),
    DecodedList = decode(Data, TypeList),
    {Array, [DataLeft]} = lists:split(ListLen, DecodedList),
    {Array, DataLeft}.

%% =====数据类型映射======
%% Erlang        其他语言
%% Integer       Integer
%% Float         Float
%% Binary        String
%% Tuple         Hash
%% List          Array

%% Erlang数据类型编码
%% 字符串支持: 仅支持BinaryString, 不支持String
encode(Info) when is_integer(Info) ->
    encode_integer(Info);
encode(Info) when is_float(Info) ->
    encode_integer(Info);
encode(Info) when is_atom(Info) ->
    encode_string(atom_to_binary(Info, utf8));
%% 二进制字符串
encode(Info) when is_binary(Info) ->
    encode_string(Info);
encode(Info) when is_tuple(Info) ->
    encode_tuple(Info);
encode(Info) when is_list(Info) ->
    encode_list(Info).

%%数据解码
decode(<<Data/binary>>, DecodeRuleList) when is_tuple(DecodeRuleList) ->
  DecodedList = decode(Data, tuple_to_list(DecodeRuleList)),
  list_to_tuple(lists:delete(<<>>, DecodedList));
decode(<<>>, []) ->
    [<<>>];
decode(<<Data/binary>>, []) ->
    [Data];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when DecodeRule == integer ->
    {Integer, DataLeft} = decode_integer(Data),
    [Integer | decode(DataLeft, DecodeRuleList)];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when DecodeRule == float ->
    {Float, DataLeft} = decode_float(Data),
    [Float | decode(DataLeft, DecodeRuleList)];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when DecodeRule == string ->
    {String, DataLeft} = decode_string(Data),
    [String | decode(DataLeft, DecodeRuleList)];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when is_tuple(DecodeRule) ->
    {Array, DataLeft} = decode_tuple(Data, DecodeRule),
    [Array | decode(DataLeft, DecodeRuleList)];
decode(<<Data/binary>>, [DecodeRule | DecodeRuleList]) when is_list(DecodeRule) ->
    {Array, DataLeft} = decode_list(Data, DecodeRule),
    [Array | decode(DataLeft, DecodeRuleList)].
