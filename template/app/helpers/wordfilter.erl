%% -*- coding: utf-8 -*-
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

-module(wordfilter).
-export([is_clean/1, clean/1]).
-include("../app/generates/wordfilter.hrl").

-define(STAR, 42). %% "*"

is_clean(BinString) ->
    S = to_string(BinString),
    {_NewString, BadWords, _Cache, _Node} = do_clean(S),
    BadWords =:= [].

clean(BinString) ->
    S = to_string(BinString),
    {NewString, _BadWords, Cache, _Node} = do_clean(S),
    Result = unicode:characters_to_binary(lists:reverse(NewString) ++ lists:reverse(Cache)),
    Result.

do_clean(String) ->
    do_clean(String, {[], [], [], ?WORD_FILTER}).

do_clean([Char], V) ->
    mark_dirty(Char, undefined, V);
do_clean([Char, NextChar|String], V) ->
    NewV = mark_dirty(Char, NextChar, V),
    do_clean([NextChar|String], NewV).

mark_dirty(Char, NextChar, {NewString, BadWords, Cache, Node}) ->
    SChar = [Char],
    case lists:keyfind(SChar, 1, Node) of
        {SChar, [{"stop", stop}]} ->
            case is_filter_valid(NextChar, Cache) of
                true ->
                    Filtered = lists:duplicate(length(Cache) + 1, ?STAR),
                    {Filtered ++ NewString, [Cache|BadWords], [], ?WORD_FILTER};
                false ->
                    {[Char|Cache] ++ NewString, BadWords, [], ?WORD_FILTER}
            end;
        {SChar, SubNode} ->
            case lists:member({"stop", stop}, SubNode) of
                true ->
                    case is_filter_valid(NextChar, Cache) of
                        true ->
                            Filtered = lists:duplicate(length(Cache) + 1, ?STAR),
                            {Filtered ++ NewString, [Cache|BadWords], [], ?WORD_FILTER};
                        false ->
                            {[Char|Cache] ++ NewString, BadWords, [], ?WORD_FILTER}
                    end;
                false ->
                    {NewString, BadWords, [Char|Cache], SubNode}
            end;
        false -> 
            case Cache of
                [] -> {[Char|NewString], BadWords, [], ?WORD_FILTER};
                _ -> {[Char|Cache] ++ NewString, BadWords, [], ?WORD_FILTER}
            end
    end.

is_english(Char) ->
    Char >= 65 andalso Char =< 122.

is_filter_valid(undefined, _BadWord) -> true;
is_filter_valid(NextChar, BadWord) ->
    FirstElement = lists:last(BadWord),
    case is_english(FirstElement) of
        true ->
            case is_english(NextChar) of
                true -> false;
                false -> true
            end;
        false -> true
    end.

to_string(BinString) ->
    [S] = io_lib:format("~ts", [BinString]),
    S.
