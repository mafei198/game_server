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


-module(game_server_app).
-behaviour(application).

-export([start/2]).
-export([prep_stop/1, stop/1]).

-include("../app/include/config_data_names.hrl").
-define(DEFAULT_REDIS_POOL_SIZE, 20).

start(_Type, _Args) ->
    ensure_started(data_holder),

    data_holder:add_ets(online_players, [named_table, public, {keypos, 2}]),
    data_holder:add_ets(game_tmp_data, [named_table, public, {keypos, 2},
                                        {read_concurrency, true}, {write_concurrency, true}]),
    data_holder:add_ets(db_counter, [named_table, public]),
    data_holder:add_ets(db_persister, [named_table, public, ordered_set]),
    data_holder:add_ets(uuid_factory, [named_table, public]),
    data_holder:add_ets(player_session, [named_table, public, {read_concurrency, true}]),
    data_holder:add_ets(chat_channel, [named_table, public, bag, {read_concurrency, true}]),
    data_holder:add_ets(chat_server, [set, public, named_table, {keypos, 2},
                                      {read_concurrency, true}]),
    data_holder:add_ets(chat_server_subscribers, [named_table, public, bag,
                                                  {read_concurrency, true}]),
    data_holder:add_ets(chat_server_trans, [named_table, public, {read_concurrency, true}]),
    data_holder:add_ets(game_log, [named_table, public, ordered_set]),
    data_holder:add_ets(iap_server, [named_table, public, ordered_set]),
    data_holder:add_ets(timertask, [set, public, named_table, {keypos, 1}]),

    configuration:init(gs_config, application:get_all_env(game_server)),
    error_logger:info_msg("gs_config: ~p~n", [configuration:all(gs_config)]),
    ibrowse:start(),
    ensure_started(eredis_pool),
    {Host, Port, DB} = misc_utils:get_redis_config(),
    PoolSize = configuration:get(gs_config, redis_pool_size, ?DEFAULT_REDIS_POOL_SIZE),
    eredis_pool:create_pool(redis, PoolSize, Host, Port, DB),

    ensure_started(crypto),
    ensure_started(asn1),
    ensure_started(inets),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(yamerl),
    Environment = configuration:get(gs_config, server_environment),
    case Environment of
        test -> ok;
        development -> lager:start();
        production -> lager:start()
    end,
    ensure_started(gproc),
    Path = case Environment of
               test -> "../../app/server_engine/config/database.yml";
               _ -> "../app/server_engine/config/database.yml"
           end,
    [DBConfList] = yamerl_constr:file(Path),
    DB_Config = case Environment of
        production -> proplists:get_value("production", DBConfList);
        development -> proplists:get_value("development", DBConfList);
        test -> proplists:get_value("test", DBConfList)
    end,
    ensure_started(db),
    db:init_pool(DB_Config),
    case Environment of
        test -> db:delete_all(schema_persistances);
        _ -> ok
    end,
    ensure_started(player_server),
    ensure_started(leaderboard),

    life_cycle:before_start(),
    R = game_server_sup:start_link(),

    shared_data:new(ws_globals, [named_table, public, {keypos, 2}, 
                                 {read_concurrency, true}]),
    shared_data:new(ws_expire_globals, [named_table, public, {keypos, 2}, 
                                        {read_concurrency, true}]),
    shared_data:new(ws_sessions, [named_table, public, {keypos, 2}, 
                                  {read_concurrency, true}]),
    shared_data:new(request, [named_table, public, {keypos, 2}, 
                              {read_concurrency, true}]),
    case Environment of
        test -> ok;
        _ -> game_numerical:load_data(?CONFIG_DATA_NAMES)
    end,
    %% start APN at last
    start_apn_application(),
    life_cycle:after_start(),
    mtwist:seed(time_utils:now()),
    rand:seed(exs1024, erlang:timestamp()),
    daily_alarm_helper:start(),
    timertask:start_check_timer(),
    game_server:set_enable(true),
    error_logger:info_msg(">>>>>>>>>>>>>>>>>>>>Server started!~n"),
    ensure_started(erlcron),
    R.

prep_stop(State) ->
    life_cycle:before_stop(),
    State.

stop(_State) ->
    life_cycle:after_stop(),
    ok.

-spec ensure_started(module()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

start_apn_application() ->
    PRO_APN_GATEAY = "gateway.push.apple.com",
    PRO_APN_FEEDBACK = "feedback.push.apple.com",
    DEV_APN_GATEAY = "gateway.sandbox.push.apple.com",
    DEV_APN_FEEDBACK = "feedback.sandbox.push.apple.com",
    ServerEnv = configuration:get(gs_config, server_environment),
    case configuration:get(gs_config, apn_env, ServerEnv) of
        production ->
            Path = filename:absname("../app/certificates/apns_production.pem"),
            case filelib:is_file(Path) of
                true -> setup_apns(PRO_APN_GATEAY, PRO_APN_FEEDBACK, Path);
                false -> ok
            end;
        development ->
            Path = filename:absname("../app/certificates/apns_development.pem"),
            case filelib:is_file(Path) of
                true -> setup_apns(DEV_APN_GATEAY, DEV_APN_FEEDBACK, Path);
                false -> ok
            end;
        test -> ok
    end.

setup_apns(Gateway, Feedback, Path) ->
    apns:start(),
    application:set_env(apns, apple_host, Gateway),
    application:set_env(apns, feedback_host, Feedback),
    application:set_env(apns, cert_file, Path).
