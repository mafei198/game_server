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


-module(player).

-behaviour(gen_server).

%% API
-export([start_link/1,
         stop/1,
         stop_conn/1,
         sync_stop/1,
         request/4,
         response/2,
         current_req/0,
         async_wait/0,
         send_data/2,
         save_data/1,
         sync_save_data/1,
         player_pid/1,
         proxy/4,
         async_proxy/4,
         wrap/2,
         async_wrap/2,
         force_async_wrap/2,
         publish/3,
         subscribe/2,
         unsubscribe/2,
         on_tcp_closed/1,
         register_tcp_closed_callback/4,
         cancel_tcp_closed_callback/2,
         add_local_timer/4,
         del_local_timer/2
        ]).

-export([clean_all_sessions/0,
         create_session/2,
         get_session/1,
         get_session_ip/1,
         del_session/1,
         conn_alive/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([dump/1]).

-record(player_state, {playerID,
                       requests,
                       expire_duration,
                       persist_duration,
                       circulation_persist_timer}).

-define(PERSIST_DURATION, 360000). %% 6 minutes, 单位(毫秒)
-define(EXPIRE_DURATION,  3600). %% 1 hours, 单位(秒)

-include("player_macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(PlayerID) ->
    gen_server:start_link(?MODULE, [PlayerID], []).

stop(PlayerID) ->
    case ?GET_PID({player, PlayerID}) of
        undefined -> ok;
        PlayerPID -> gen_server:cast(PlayerPID, {stop, shutdown})
    end.

stop_conn(PlayerID) ->
    case con_pid(PlayerID) of
        undefined -> do_nothing;
        ConPid -> msg_server:stop(ConPid)
    end,
    case ?GET_PID({player, PlayerID}) of
        undefined -> ok;
        PlayerPID -> gen_server:cast(PlayerPID, {stop, shutdown})
    end.

sync_stop(PlayerID) ->
    case ?GET_PID({player, PlayerID}) of
        undefined -> ok;
        PlayerPID -> gen_server:call(PlayerPID, {stop, shutdown})
    end.

request(PlayerID, Path, Params, RequestId) ->
    gen_server:cast(player_pid(PlayerID), {request, Path, Params, RequestId}).

current_req() ->
    get({?MODULE, current_req}).

async_wait() ->
    {req, async_wait}.

response(_Req = {PlayerID, RequestId}, Response) ->
    gen_server:cast(player_pid(PlayerID), {response, RequestId, Response}).

send_data(PlayerID, Data) ->
    case validate_ownership(PlayerID) of
        true -> 
            pending_response(0, Data),
            case is_requesting() of
                true -> ok;
                false -> notify_flush_response()
            end;
        false ->
            send_data_to_connection(PlayerID, Data)
    end.

save_data(PlayerID) ->
    case validate_ownership(PlayerID) of
        true -> 
            model:persist_all();
        false ->
            gen_server:cast(player_pid(PlayerID), {save_data})
    end.

sync_save_data(PlayerID) ->
    case validate_ownership(PlayerID) of
        true -> model:persist_all();
        false -> 
            gen_server:call(player_pid(PlayerID), {sync_save_data})
    end.

proxy(PlayerID, Module, Fun, Args) ->
    try do_proxy(PlayerID, Module, Fun, Args) of
        R -> R
    catch
        exit:{{shutdown, data_persisted}, _} ->
            proxy(PlayerID, Module, Fun, Args)
    end.

do_proxy(PlayerID, Module, Fun, Args) ->
    case validate_ownership(PlayerID) of
        true ->
            track_active(),
            apply(Module, Fun, Args);
        false ->
            gen_server:call(player_pid(PlayerID), {proxy, Module, Fun, Args})
    end.

async_proxy(PlayerID, Module, Fun, Args) ->
    gen_server:cast(player_pid(PlayerID), {proxy, Module, Fun, Args}).

wrap(PlayerID, Fun) ->
    try do_wrap(PlayerID, Fun) of
        R -> R
    catch
        exit:{{shutdown, data_persisted}, _} ->
            wrap(PlayerID, Fun)
    end.

do_wrap(PlayerID, Fun) ->
    case validate_ownership(PlayerID) of
        true -> 
            track_active(),
            Fun();
        false ->
            gen_server:call(player_pid(PlayerID), {wrap, Fun})
    end.

async_wrap(PlayerID, Fun) ->
    case validate_ownership(PlayerID) of
        true -> 
            track_active(),
            Fun();
        false ->
            gen_server:cast(player_pid(PlayerID), {wrap, Fun})
    end.

force_async_wrap(PlayerID, Fun) ->
    gen_server:cast(player_pid(PlayerID), {wrap, Fun}).

publish(Channel, MsgType, Msg) ->
    ?PUBLISH(Channel, {gproc_msg, MsgType, Msg}).
subscribe(PlayerID, Channel) ->
    Pid = con_pid(PlayerID),
    msg_server:subscribe(Pid, Channel).
unsubscribe(PlayerID, Channel) ->
    Pid = con_pid(PlayerID),
    msg_server:unsubscribe(Pid, Channel).

on_tcp_closed(PlayerID) ->
    gen_server:cast(player_pid(PlayerID), {on_tcp_closed}).

register_tcp_closed_callback(PlayerID, Key, Type, Fun) ->
    gen_server:cast(player_pid(PlayerID), {register_tcp_closed_callback, Key, Type, Fun}).

cancel_tcp_closed_callback(PlayerID, Key) ->
    gen_server:cast(player_pid(PlayerID), {cancel_tcp_closed_callback, Key}).

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

create_session(PlayerID, IP) ->
    ets:insert(player_session, {PlayerID, integer_to_binary(erlang:monotonic_time()), IP}).

clean_all_sessions() ->
    ets:delete_all_objects(player_session).

get_session(PlayerID) ->
    case ets:lookup(player_session, PlayerID) of
        [] -> undefined;
        [{_K, V, _IP}] -> V
    end.

get_session_ip(PlayerID) ->
    case ets:lookup(player_session, PlayerID) of
        [] -> undefined;
        [{_K, _V, IP}] -> IP
    end.

del_session(PlayerID) ->
    ets:delete(player_session, PlayerID),
    session_expired_notify(PlayerID),
    stop_conn(PlayerID).

conn_alive(PlayerID) ->
    con_pid(PlayerID) =/= undefined.

dump(PlayerID) ->
    io:format("~p~n", [erlang:process_info(player:player_pid(PlayerID))]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerID]) ->
    ?REG_PID({player, PlayerID}),
    put(player_id, PlayerID),
    Timer = erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data),
    process_flag(trap_exit, true),
    {ok, #player_state{playerID = PlayerID, 
                       requests = queue:new(),
                       expire_duration = ?EXPIRE_DURATION,
                       persist_duration = ?PERSIST_DURATION,
                       circulation_persist_timer = Timer}}.

handle_call({proxy, Module, Fun, Args}, _From, State) ->
    track_active(),
    try erlang:apply(Module, Fun, Args) of
        Result -> 
            {reply, Result, State}
    catch
        Type:Msg ->
            exception:notify(Type, {Module, Fun, Args, [<<"PlayerID">>, get(player_id)]}, Msg),
            {reply, exception, State}
    end;
handle_call({wrap, Fun}, _From, State) ->
    track_active(),
    Reply = try Fun() of
                Result -> Result
            catch
                Type:Msg ->
                    exception:notify(Type, [<<"PlayerID">>, get(player_id)], Msg),
                    exception
            end,
    {reply, Reply, State};
handle_call({sync_save_data}, _From, State) ->
    model:persist_all(),
    try db_persister:ensure_persisted(get(player_id)) of
        Result -> Result
    catch
        Type:Msg ->
            exception:notify(Type, [<<"sync_save_data failed">>], Msg)
    end,
    {reply, ok, State};
handle_call({stop, shutdown}, _From, State) ->
    model:persist_all(),
    Result = try db_persister:ensure_persisted(get(player_id)) of
                 R -> R
             catch
                 Type:Msg ->
                     exception:notify(Type, [<<"sync_save_data failed">>], Msg),
                     false
             end,
    case Result of
        true ->
            {stop, {shutdown, data_persisted}, State};
        false ->
            {reply, {fail, persist_failed}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({proxy, Module, Fun, Args}, State) ->
    track_active(),
    try erlang:apply(Module, Fun, Args) of
        _ -> ok
    catch
        Type:Msg ->
            exception:notify(Type, {Module, Fun, Args, [<<"PlayerID">>, get(player_id)]}, Msg)
    end,
    {noreply, State};
handle_cast({wrap, Fun}, State) ->
    track_active(),
    try Fun() of
        _ -> ok
    catch
        Type:Msg -> 
            exception:notify(Type, [<<"PlayerID">>, get(player_id)], Msg)
    end,
    {noreply, State};
handle_cast({request, {Controller, Action}, Params, RequestId}, State) ->
    track_active(),
    Request = {Controller, Action, Params, RequestId},
    Queue = queue:in(Request, State#player_state.requests),
    NQueue = execute_requests_queue(State#player_state.playerID, Queue),
    {noreply, State#player_state{requests = NQueue}};
handle_cast({response, RequestId, Response}, State) ->
    send_response(State#player_state.playerID, RequestId, Response),
    {noreply, State};
handle_cast({stop, shutdown}, State) ->
    model:persist_all(),
    case db_persister:ensure_persisted(get(player_id)) of
        true ->
            {stop, {shutdown, data_persisted}, State};
        false ->
            {noreply, State}
    end;
handle_cast({save_data}, State) ->
    model:persist_all(),
    {noreply, State};
handle_cast({on_tcp_closed}, State) ->
    invoke_on_tcp_closed(),
    {noreply, State};
handle_cast({register_tcp_closed_callback, Key, Type, Fun}, State) ->
    case get(tcp_closed_callback) of
        undefined -> put(tcp_closed_callback, [{Key, Type, Fun}]);
        List -> put(tcp_closed_callback, [{Key, Type, Fun}|List])
    end,
    {noreply, State};
handle_cast({cancel_tcp_closed_callback, DelKey}, State) ->
    case get(tcp_closed_callback) of
        undefined -> ok;
        Callbacks ->
            NewCallbacks = lists:foldl(fun({Key, Type, Fun}, Result) ->
                case Key =:= DelKey of
                    true -> Result;
                    false -> [{Key, Type, Fun}|Result]
                end
            end, [], Callbacks),
            put(tcp_closed_callback, NewCallbacks)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush_responses, State) ->
    flush_data_to_connection(State#player_state.playerID),
    {noreply, State};
handle_info(circulation_persist_data, State=#player_state{circulation_persist_timer=Timer}) ->
    erlang:cancel_timer(Timer),
    IsExpired = is_player_expired(State),
    IsPersistFinished = make_sure_persisted(State#player_state.playerID),
    if
        IsExpired andalso IsPersistFinished ->
            {stop, {shutdown, data_persisted}, State};
        true -> 
            Duration = State#player_state.persist_duration,
            NewTimer = erlang:send_after(Duration, self(), circulation_persist_data),
            {noreply, State#player_state{circulation_persist_timer=NewTimer}}
    end;
handle_info({local_timer, Key, {M, F, A}}, State=#player_state{playerID=PlayerID}) ->
    erase({local_timer_data, Key}),
    case con_pid(PlayerID) of
        undefined -> ok;
        _ -> try erlang:apply(M, F, A) of
                 R -> R
             catch
                 Type:Msg ->
                     exception:notify(Type, [M, F, A], Msg)
             end
    end,
    {noreply, State};
handle_info({shutdown, From}, State) ->
    model:persist_all(),
    From ! {finished_shutdown, self()},
    {stop, {shutdown, data_persisted}, State};
handle_info(Info, State) ->
    error_logger:info_msg("Player dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State=#player_state{playerID=PlayerID, circulation_persist_timer=Timer}) ->
    invoke_on_tcp_closed(),
    error_logger:info_msg("Player: ~p, Terminate With Reason: ~p~n", [PlayerID, Reason]),
    case Reason of
        {shutdown, data_persisted} -> ok;
        _ -> model:persist_all()
    end,
    erlang:cancel_timer(Timer),
    redis_pool:del_redis({player, PlayerID}),
    del_session(PlayerID),
    gproc:goodbye(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

player_pid(PlayerID) when is_binary(PlayerID) andalso PlayerID =/= <<"">> ->
    case ?GET_PID({player, PlayerID}) of
        undefined -> 
            % logger:info("player, will start player from player_factory~n"),
            {ok, Pid} = player_factory:start_player(PlayerID),
            Pid;
        Pid -> 
            % logger:info("player, get player_pid from gproc~n"),
            Pid
    end.

con_pid(PlayerID) ->
    ?GET_PID({connection, PlayerID}).

track_active() ->
    put({player, last_active}, time_utils:current_time()).

get_last_active() ->
    case get({player, last_active}) of
        undefined -> 0;
        LastActive -> LastActive
    end.

validate_ownership(PlayerID) ->
    PlayerID =:= get(player_id).

send_data_to_connection(PlayerID, Data) ->
    send_data_to_connection(PlayerID, Data, false).

send_data_to_connection(PlayerID, Data, IsMulti) ->
    case con_pid(PlayerID) of
        undefined -> do_nothing;
        ConPid ->
            if
                IsMulti ->
                    msg_server:send_multi_data(ConPid, Data);
                true ->
                    msg_server:send_data(ConPid, Data)
            end
    end.

is_requesting() ->
    case get({status, is_requesting}) of
        undefined -> false;
        true -> true
    end.

begin_request(PlayerID, RequestId) ->
    put({status, is_requesting}, true),
    put({?MODULE, current_req}, {PlayerID, RequestId}).

finish_request() ->
    erase({status, is_requesting}),
    erase({?MODULE, current_req}).

invoke_on_tcp_closed() ->
    case get(tcp_closed_callback) of
        undefined -> ok;
        Callbacks ->
            NewCallbacks = lists:foldl(fun({Key, Type, Fun}, Result) ->
                try Fun() of
                    _Response -> ok
                catch
                    Exception:Msg ->
                        exception:notify(Exception, [<<"PlayerID">>, get(player_id)], Msg)
                end,
                case Type of
                    callback_once -> Result;
                    callback_ever -> [{Key, Type, Fun}|Result]
                end
            end, [], Callbacks),
            put(tcp_closed_callback, NewCallbacks)
    end.

pending_response(RequestId, Data) ->
    CachedData = case get(pending_responses) of
        undefined ->  [];
        PendingResponses -> PendingResponses
    end,
    put(pending_responses, [{RequestId, Data}|CachedData]).

get_pending_responses() ->
    case get(pending_responses) of
        undefined -> [];
        CachedResponses -> CachedResponses
    end.

del_pending_responses() ->
    erase(pending_responses).

flush_data_to_connection(PlayerID) ->
    CachedData = get_pending_responses(),
    del_pending_responses(),
    if
        CachedData =/= [] ->
            send_data_to_connection(PlayerID, lists:reverse(CachedData), true),
            cancel_notify_flush();
        true -> ok
    end.

notify_flush_response() ->
    Key = '__schedule_notify__',
    case get(Key) of
        undefined -> 
            TRef = erlang:send_after(50, self(), flush_responses),
            put(Key, TRef);
        _TRef -> ok
    end.

cancel_notify_flush() ->
    Key = '__schedule_notify__',
    case erase(Key) of
        undefined -> ok;
        TRef -> erlang:cancel_timer(TRef)
    end.

session_expired_notify(PlayerID) ->
    case erlang:function_exported(users_model, session_expired_callback, 1) of
        true -> users_model:session_expired_callback(PlayerID);
        false -> ok
    end.

make_sure_persisted(PlayerID) ->
    model:persist_all(),
    try db_persister:is_persist_finished(PlayerID) of
        R -> R
    catch
        _:_ -> false
    end.

is_player_expired(State) ->
    IdleSeconds = time_utils:current_time() - get_last_active(),
    IdleSeconds >= State#player_state.expire_duration.

execute_request(PlayerID, Controller, Action, Params) ->
    try Controller:Action(PlayerID, Params) of
        Resp -> Resp
    catch
        Type:Msg ->
            exception:notify(Type, Msg, Controller, Action, 
                             [{playerID, PlayerID}|Params]),
            {fail, error_internal_error}
    end.

send_response(PlayerID, RequestId, Response) ->
    pending_response(RequestId, Response),
    flush_data_to_connection(PlayerID),
    finish_request().

execute_requests_queue(PlayerID, Queue) ->
    case is_requesting() of
        true -> Queue;
        false ->
            case queue:out(Queue) of
                {empty, Queue} -> Queue;
                {{value, {Controller, Action, Params, RequestId}}, Queue1} ->
                    begin_request(PlayerID, RequestId),
                    case execute_request(PlayerID, Controller, Action, Params) of
                        {req, async_wait} -> ok;
                        Response -> send_response(PlayerID, RequestId, Response)
                    end,
                    Queue1
            end
    end.
