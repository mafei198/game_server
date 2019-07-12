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


-module(player_data_tests).
-include_lib("eunit/include/eunit.hrl").
-include ("include/db_schema.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define (UDID, <<"eunit_self_udid">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
player_data_api_test() ->
% player_data_api_test_() ->
    % [{"Player Data API test.", ?setup(fun tests/1)}].
    {foreach,
     fun start/0,
     fun stop/1,
     [fun create_test/1,
      fun delete_test/1,
      fun update_test/1,
      fun search_test/1,
      fun count_test/1,
      fun clean_test/1,
      fun record_status_test/1
     ]}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    game_server:start([test]),
    redis:delete_all(users).

stop(_Pid) ->
    game_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
create_test(_Pid) ->
    redis:delete_all(users),
    {ok, []} = db:all(users),
    NewUdid = <<"new_user_udid">>,
    NewName = <<"new_user_name">>,
    PlayerID = player_data:get_player_id(?UDID),
    NewPlayerID = player_data:get_player_id(NewUdid),

    player:proxy(NewPlayerID, player_data, delete, [NewPlayerID, #users{udid = NewUdid}]),
    player:proxy(PlayerID, player_data, flush_to_mysql, []),
    EmptyUser = player_data:find(PlayerID, #users{udid = NewUdid}),
    player:proxy(PlayerID, player_data, create, [PlayerID, #users{udid = NewUdid,
                                                                  name = NewName}]),

    R = player_data:find(PlayerID, #users{udid = NewUdid}),

    player:proxy(PlayerID, player_data, flush_to_mysql, []),
    [] = player_data:all_record_status(),
    {ok, [DBUser]} = db:find_by(users, udid, NewUdid),
    Count = player_data:count(PlayerID, #users{udid = NewUdid}),

    [?_assert(erlang:is_binary(PlayerID)),
     ?_assertEqual(R#users.udid, NewUdid),
     ?_assertNotEqual(PlayerID, <<"">>),
     ?_assertEqual(EmptyUser, undefined),
     ?_assertEqual(Count, 1),
     ?_assertEqual(DBUser#users.name, NewName),
     ?_assertEqual(R#users.name, NewName)
    ].

delete_test(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    player_data:find(PlayerID, #users{udid = ?UDID}), %% load user from db to ets
    player:proxy(PlayerID, player_data, delete, [PlayerID, #users{udid = ?UDID}]),
    NewUser = player_data:find(PlayerID, #users{udid = ?UDID}),
    player_data:flush_to_mysql(),
    {ok, Users} = db:find_by(users, udid, ?UDID),
    [?_assertEqual(NewUser, undefined),
     ?_assertEqual(Users, [])].

update_test(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    User = player_data:find(PlayerID, #users{udid = ?UDID}), %% load user from db to ets
    player:proxy(PlayerID, player_data, update, [PlayerID, #users{udid = ?UDID}, #users{name = <<"new savin test">>}]),
    NewUser = player_data:find(PlayerID, #users{udid = ?UDID}),
    player_data:flush_to_mysql(),
    {ok, [DBUser]} = db:find_by(users, udid, ?UDID),
    [?_assertEqual(NewUser#users.name, <<"new savin test">>),
     ?_assertNotEqual(User#users.name, NewUser#users.name),
     ?_assertEqual(DBUser#users.name, NewUser#users.name)
    ].

search_test(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    Users = player_data:where(PlayerID, #users{udid = ?UDID}),
    User  = player_data:find(PlayerID, #users{udid = ?UDID}),
    [?_assertEqual(Users, [User]),
     ?_assertNotEqual(Users, []),
     ?_assertNotEqual(User, undefined)
    ].

count_test(_Pid) ->
    redis:delete_all(users),
    PlayerID = player_data:get_player_id(?UDID),
    NewUdid = <<"new_user_udid">>,
    C1 = player_data:count(PlayerID, #users{udid = ?UDID}),
    player:proxy(PlayerID, player_data, create, [PlayerID, #users{udid = NewUdid}]),
    C2 = player_data:count(PlayerID, #users{}),
    player:proxy(PlayerID, player_data, delete, [PlayerID, #users{udid = NewUdid}]),
    C3 = player_data:count(PlayerID, #users{}),
    [?_assertEqual(C1, 1),
     ?_assertEqual(C2, 2),
     ?_assertEqual(C3, 1)].

clean_test(_Pid) ->
    redis:delete_all(users),
    PlayerID = player_data:get_player_id(?UDID),
    _User = player_data:find(PlayerID, #users{udid = ?UDID}),
    player:proxy(PlayerID, player_data, clean, [PlayerID, users]),
    Loaded = player_data:get_loaded(PlayerID, users),
    User = player_data:ets_find(#users{udid = ?UDID}),
    [?_assertNotEqual(Loaded, true),
     ?_assertEqual(User, undefined)
    ].


record_status_test(_Pid) ->
    %get_single_record_status/3,
    %get_player_record_status/2,
    %get_player_records_status/1,
    %Status: delete, update, create
    redis:delete_all(users),
    PlayerID = player_data:get_player_id(?UDID),
    Selector = #users{uuid = PlayerID},
    Modifier = #users{name = <<"new name">>},

    player:proxy(PlayerID, player_data, update, [PlayerID, Selector, Modifier]),
    {UpdateStatus, UpdateValue} = player_data:get_single_record_status(
                                    PlayerID, users, PlayerID),
    [[PlayerID, update, [name]]] = player_data:get_player_record_status(PlayerID, users),
    [[users, PlayerID, update, [name]]] = player_data:get_player_records_status(PlayerID),

    player:proxy(PlayerID, player_data, delete, [PlayerID, Selector]),
    {DeleteStatus, DeleteValue} = player_data:get_single_record_status(
                                    PlayerID, users, PlayerID),
    [[PlayerID, delete, undefined]] = player_data:get_player_record_status(PlayerID, users),
    [[users, PlayerID, delete, undefined]] = player_data:get_player_records_status(PlayerID),

    CModifier = #users{name = <<"new name">>},
    player:proxy(PlayerID, player_data, create, [PlayerID, CModifier]),
    NewPlayer = player_data:ets_find(Modifier),
    NewPlayerID = NewPlayer#users.uuid,
    {CreateStatus, CreateValue} = player_data:get_single_record_status(
                                    NewPlayerID, users, NewPlayerID),
    [[NewPlayerID, create, undefined]] = player_data:get_player_record_status(NewPlayerID, users),
    [[users, NewPlayerID, create, undefined]] = player_data:get_player_records_status(NewPlayerID),

    [?_assertEqual(UpdateStatus, update),
     ?_assertEqual(DeleteStatus, delete),
     ?_assertEqual(CreateStatus, create),

     ?_assertEqual(UpdateValue, [name]),
     ?_assertEqual(DeleteValue, undefined),
     ?_assertEqual(CreateValue, undefined)
    ].



%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
