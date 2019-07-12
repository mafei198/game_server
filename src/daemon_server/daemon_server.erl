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


-module(daemon_server).

-behaviour(gen_server).

%% API
-export([start_link/1,
         player_pid/1,
         stop/1,
         proxy/4,
         async_proxy/4,
         wrap/2,
         async_wrap/2,
         force_async_wrap/2,
         add_local_timer/4,
         del_local_timer/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([dump/1]).

-record(state, {playerID}).

-define(PERSIST_DURATION, 360000). %% 6 minutes, 单位(毫秒)
-define(EXPIRE_DURATION,  3600). %% 1 hours, 单位(秒)

-include("include/gproc_macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(PlayerID) ->
    gen_server:start_link(?MODULE, [PlayerID], []).

stop(PlayerID) ->
    gen_server:cast(player_pid(PlayerID), {stop}).
    
proxy(PlayerID, Module, Fun, Args) ->
    case validate_ownership(PlayerID) of
        true ->
            apply(Module, Fun, Args);
        false ->
            gen_server:call(player_pid(PlayerID), {proxy, Module, Fun, Args})
    end.

async_proxy(PlayerID, Module, Fun, Args) ->
    gen_server:cast(player_pid(PlayerID), {proxy, Module, Fun, Args}).

wrap(PlayerID, Fun) ->
    case validate_ownership(PlayerID) of
        true -> 
            Fun();
        false ->
            gen_server:call(player_pid(PlayerID), {wrap, Fun})
    end.

async_wrap(PlayerID, Fun) ->
    case validate_ownership(PlayerID) of
        true -> 
            Fun();
        false ->
            gen_server:cast(player_pid(PlayerID), {wrap, Fun})
    end.

force_async_wrap(PlayerID, Fun) ->
    gen_server:cast(player_pid(PlayerID), {wrap, Fun}).

add_local_timer(PlayerID, Key, SendAfterSecs, MFA) ->
    player:wrap(PlayerID, fun() ->
        case get({local_timer_data, Key}) of
            undefined -> ok;
            OldTimer -> erlang:cancel_timer(OldTimer)
        end,
        RunAt = SendAfterSecs * 1000,
        Timer = erlang:send_after(RunAt, self(), {local_timer, Key, MFA}),
        put({local_timer_data, Key}, Timer)
    end).

del_local_timer(PlayerID, Key) ->
    player:wrap(PlayerID, fun() ->
        case get({local_timer_data, Key}) of
            undefined -> ok;
            Timer -> erlang:cancel_timer(Timer)
        end
    end).

dump(PlayerID) ->
    io:format("~p~n", [erlang:process_info(daemon_server:player_pid(PlayerID))]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerID]) ->
    ?REG_PID({daemon_server, PlayerID}),
    put(daemon_server_id, PlayerID),
    process_flag(trap_exit, true),
    {ok, #state{playerID=PlayerID}}.

handle_call({proxy, Module, Fun, Args}, _From, State) ->
    try erlang:apply(Module, Fun, Args) of
        Result -> 
            {reply, Result, State}
    catch
        Type:Msg ->
            exception:notify(Type, {Module, Fun, Args, [<<"PlayerID">>, get(daemon_server_id)]}, Msg),
            {reply, exception, State}
    end;
handle_call({wrap, Fun}, _From, State) ->
    Reply = try Fun() of
                Result -> Result
            catch
                Type:Msg ->
                    exception:notify(Type, [<<"PlayerID">>, get(daemon_server_id)], Msg),
                    exception
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({proxy, Module, Fun, Args}, State) ->
    try erlang:apply(Module, Fun, Args) of
        _ -> ok
    catch
        Type:Msg ->
            exception:notify(Type, {Module, Fun, Args, 
                                    [<<"DeamonServerId">>, get(daemon_server_id)]}, Msg)
    end,
    {noreply, State};
handle_cast({wrap, Fun}, State) ->
    try Fun() of
        _ -> ok
    catch
        Type:Msg -> 
            exception:notify(Type, [<<"DeamonServerId">>, get(daemon_server_id)], Msg)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({local_timer, Key, {M, F, A}}, State) ->
    erase({local_timer_data, Key}),
    try erlang:apply(M, F, A) of
        R -> R
    catch
        Type:Msg ->
            exception:notify(Type, [M, F, A], Msg)
    end,
    {noreply, State};
handle_info({stop}, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    error_logger:info_msg("Daemon Server dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State=#state{playerID=PlayerID}) ->
    logger:info("daemon_server terminate: ~p, Id: ~p~n", [Reason, PlayerID]),
    gproc:goodbye(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

player_pid(PlayerID) when is_binary(PlayerID) andalso PlayerID =/= <<"">> ->
    case ?GET_PID({?MODULE, PlayerID}) of
        undefined -> 
            {ok, Pid} = daemon_server_factory:start_daemon(PlayerID),
            Pid;
        Pid -> Pid
    end.

validate_ownership(PlayerID) ->
    PlayerID =:= get(daemon_server_id).
