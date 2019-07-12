%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014-2024
%%% Savin Max <mafei.198@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%
%%% @doc
%%%        Binary String utils.
%%% @end
%%% Created :  二  3 11 14:01:02 2014 by Savin Max

-module(binary_string).

-export([uri_encode/1,
         uri_decode/1,
         split/2, 
         join/2, 
         is_valid_for_mysql/1, 
         clean_for_mysql/1, 
         format/2, 
         strip/1,
         is_blank/1]).

uri_encode(BinaryString) ->
    list_to_binary(http_uri:encode(binary_to_list(BinaryString))).

uri_decode(BinaryString) ->
    list_to_binary(http_uri:decode(binary_to_list(BinaryString))).

strip(BinaryString) ->
    re:replace(BinaryString, "(^\\s+)|(\\s+$)", "", [global,{return,binary}]).

is_blank(BinaryString) ->
    BinaryString =:= <<>> orelse BinaryString =:= undefined.

split(BinaryString, Separator) ->
    split(BinaryString, Separator, []).

split(BinaryString, Separator, Result) ->
    case binary:split(BinaryString, Separator) of
        [BinaryString] ->
            lists:reverse([BinaryString|Result]);
        [Head, RemainBinaryString] ->
            split(RemainBinaryString, Separator, [Head|Result])
    end.

join(BinaryStringList, Separator) ->
    join(BinaryStringList, Separator, <<>>).

%%% Private Methods
join([], _Separator, Result) ->
    Result;
join([BinaryString|BinaryStringList], Separator, <<>>) ->
    join(BinaryStringList, Separator, BinaryString);
join([BinaryString|BinaryStringList], Separator, Result) ->
    join(BinaryStringList, Separator, <<Result/binary, Separator/binary, BinaryString/binary>>).


-define(INVALID_MYSQL_UTF8_RE, "[\x{010000}-\x{10FFFF}]").

is_valid_for_mysql(BinString) ->
    case re:run(unicode:characters_to_list(BinString), ?INVALID_MYSQL_UTF8_RE, [unicode]) of
        nomatch -> true;
        _ -> false
    end.

clean_for_mysql(BinString) ->
    re:replace(unicode:characters_to_list(BinString), ?INVALID_MYSQL_UTF8_RE, "", [global, unicode, {return, binary}]).

%% Usage:
%% binary_string:format(<<"hello [name], are you [age] years old?">>, 
%%                      [{name, <<"savin">>}, {age, 26}]).
format(String, List) ->
    case re:run(String, "\\[[a-z0-9_]+\\]", [global]) of
        {match, Matches} ->
            % logger:info("Matches: ~p~n", [Matches]),
            Values = lists:foldl(fun([{Pos, Len}], Result) ->
                Key = binary:part(String, Pos + 1, Len - 2),
                % logger:info("Matches: ~p, Key: ~p, String: ~p, List: ~p~n",
                %             [Matches, Key, String, List]),
                {_, Value} = lists:keyfind(binary_to_atom(Key, utf8), 1, List),
                [convert_value_to_binary(Value)|Result]
            end, [], Matches),
            RValues = lists:reverse(Values),
            RvalueLen = length(RValues),
            {Formated, _} = lists:foldl(fun(Chunk, {Result, Idx}) ->
                % logger:info("Chunk: ~p~n", [Chunk]),
                if
                    Idx =< RvalueLen ->
                        {[lists:nth(Idx, RValues)|[Chunk|Result]], Idx + 1};
                    true ->
                        {[Chunk|Result], Idx + 1}
                end
            end, {[], 1}, re:split(String, "\\[[a-z0-9_]+\\]")),
            list_to_binary(lists:reverse(Formated));
        nomatch -> String
    end.

convert_value_to_binary(Value) when is_binary(Value) -> Value;
convert_value_to_binary(Value) when is_list(Value) -> list_to_binary(Value);
convert_value_to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
convert_value_to_binary(Value) when is_float(Value) -> float_to_binary(Value);
convert_value_to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8).
