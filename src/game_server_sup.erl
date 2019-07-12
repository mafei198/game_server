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


-module(game_server_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0, get_redis_config/0]).

%% supervisor.
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    GameNumericalSpec = ?CHILD(game_numerical, game_numerical, worker, []),
    RedisConfig = get_redis_config(),
    RedisPoolSupSpec = ?CHILD(redis_pool_sup, redis_pool_sup, supervisor, [RedisConfig]),
    GamePort = configuration:get(gs_config, game_port, 5271),
    APIPort = configuration:get(gs_config, api_port, 6666),
    error_logger:info_msg("GamePort: ~p, APIPort: ~p~n", [GamePort, APIPort]),
    WorldServerSupSpec = ?CHILD(shared_data_sup, shared_data_sup, supervisor, []),
    RanchSupSpec = ?CHILD(ranch_sup, ranch_sup, supervisor, []),
    ListenOpts = [binary,
                  {active, false},
                  {packet, raw},
                  {reuseaddr, true},
                  {nodelay, true},
                  {send_timeout_close, true},
                  {backlog, 1024},
                  {port, GamePort},
                  {send_timeout, 3000},
                  {keepalive, true}],
    ListenerSpec = ?CHILD(tcp_acceptor_sup, tcp_acceptor_sup, supervisor, [ListenOpts]),
    MsgServerSupSpec = ?CHILD(msg_server_sup, msg_server_sup, supervisor, []),
    ApiServerSpec = ranch:child_spec(api_tcp_listener, 1,
        ranch_tcp, [{port, APIPort}], api_connection, []
    ),
    GameLog = ?CHILD(game_log_sup, game_log_sup, supervisor, []),
    GameServerSpec = ?CHILD(game_server, game_server, worker, []),
    NameServerSupSpec = ?CHILD(name_server_sup, name_server_sup, supervisor, []),
    IapServerSupSpec = ?CHILD(iap_server_sup, iap_server_sup, supervisor, []),
    HttpWorkerSpec = ?CHILD(http, http, worker, []),
    ChatServerSupSpec = ?CHILD(chat_server_sup, chat_server_sup, supervisor, []),
    TimertaskSupSpec = ?CHILD(timertask_sup, timertask_sup, supervisor, []),
    DaemonServerSupSpec = ?CHILD(daemon_server_sup, daemon_server_sup, supervisor, []),
    DaemonServerFactorySpec = ?CHILD(daemon_server_factory, daemon_server_factory, worker, []),
    BsBotSupSPec = ?CHILD(bs_bot_sup, bs_bot_sup, supervisor, []),
    FCMSupSpec = ?CHILD(fcm_sup, fcm_sup, supervisor, []),
    Specs = [GameNumericalSpec, WorldServerSupSpec, RedisPoolSupSpec, 
             GameLog, GameServerSpec, RanchSupSpec, 
             ListenerSpec, MsgServerSupSpec, ApiServerSpec, 
             NameServerSupSpec, IapServerSupSpec, HttpWorkerSpec, 
             ChatServerSupSpec, DaemonServerFactorySpec, DaemonServerSupSpec, 
             TimertaskSupSpec, BsBotSupSPec, FCMSupSpec],
    NSpecs = case configuration:get(gs_config, enable_hot_code_reload) of
                 true -> 
                     [?CHILD(reloader, reloader, worker, [])|Specs];
                 _ -> Specs
             end,
    {ok, {{one_for_one, 10, 10}, NSpecs}}.

%% Private
get_redis_config() ->
    Host = configuration:get(gs_config, redis_host, "127.0.0.1"),
    Port = configuration:get(gs_config, redis_port, 6379),
    DB   = configuration:get(gs_config, redis_db, 0),
    [Host, Port, DB].
