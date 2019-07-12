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
%%%       Manage player's ets data:
%%% @end
%%% Created :  日 10 06 23:34:35 2013 by Savin-Max µ

-module(player_data).

%% API
-export([get_player_id/1,
         create/2,
         delete/2,
         update/2,
         update/3,
         find/2,
         where/2,
         all/2,
         count/2,
         count_all/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%% Only can be invoked by game_connection
get_player_id(Udid) ->
    users_model:get_player_id(Udid).

create(PlayerID, Record) when is_tuple(Record) ->
    case validate_ownership(PlayerID) of
        true -> model:create(Record);
        false -> player:proxy(PlayerID, model, create, [Record])
    end;
create(PlayerID, Records) when is_list(Records) ->
    case validate_ownership(PlayerID) of
        true ->
            lists:foreach(fun(Record) ->
                              model:create(Record)
                          end, Records);
        false ->
            lists:foreach(fun(Record) ->
                              player:proxy(PlayerID, model, create, [Record])
                          end, Records)
    end.

delete(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:delete(Selector);
        false -> player:proxy(PlayerID, model, delete, [Selector])
    end.

update(PlayerID, NewRecord) ->
    case validate_ownership(PlayerID) of
        true -> model:update(NewRecord);
        false -> player:proxy(PlayerID, model, update, [NewRecord])
    end.

update(PlayerID, Selector, Modifier) ->
    case validate_ownership(PlayerID) of
        true -> model:update(Selector, Modifier);
        false -> player:proxy(PlayerID, model, update, [Selector, Modifier])
    end.

find(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:find(Selector);
        false -> player:proxy(PlayerID, model, find, [Selector])
    end.

where(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:where(Selector);
        false -> player:proxy(PlayerID, model, where, [Selector])
    end.

all(PlayerID, Table) ->
    case validate_ownership(PlayerID) of
        true -> model:all(Table);
        false -> player:proxy(PlayerID, model, all, [Table])
    end.

count(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:count(Selector);
        false -> player:proxy(PlayerID, model, count, [Selector])
    end.

count_all(PlayerID, Table) ->
    case validate_ownership(PlayerID) of
        true -> model:count_all(Table);
        false -> player:proxy(PlayerID, model, count_all, [Table])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
validate_ownership(PlayerID) ->
    PlayerID =:= get(player_id).
