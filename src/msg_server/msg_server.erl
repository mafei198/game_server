-module(msg_server).

-behaviour(gen_server).

%% API
-export([start_link/1, 
         controlling_process/2,
         send_data/2,
         send_data/3,
         send_multi_data/2,
         subscribe/2,
         unsubscribe/2,
         stop/1,
         sync_stop/1,
         kickout_all/0,
         sync_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% MsgServer will stop:
%%  1.when msg queue full
%%  2.inactive over 24 hours
-define(MSG_QUEUE_LEN, 100).
-define(CACHE_QUEUE_MAX_LEN, ?MSG_QUEUE_LEN).
-define(ACTIVITY_CHECK_DURATION, 3600000). %% millisecond
-define(EXPIRE_DURATION, 86400). %% seconds

-record(state, {player_id, 
                socket, 
                timer, 
                channels,
                last_active_time}).

-include("../app/include/secure.hrl").
-include ("include/db_schema.hrl").
-include ("include/common_const.hrl").
-include("include/gproc_macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link(PlayerID) ->
    gen_server:start_link(?MODULE, [PlayerID], []).

controlling_process(Pid, Socket) ->
    gen_tcp:controlling_process(Socket, Pid),
    gen_server:cast(Pid, {controlling_process, Socket}).

send_data(Pid, Data) ->
    gen_server:cast(Pid, {send_data, Data}).

send_data(Pid, RequestId, Data) ->
    gen_server:cast(Pid, {send_data, RequestId, Data}).

send_multi_data(Pid, MultiData) ->
    gen_server:cast(Pid, {send_multi_data, MultiData}).

subscribe(Pid, Channel) ->
    gen_server:cast(Pid, {subscribe, Channel}).

unsubscribe(Pid, Channel) ->
    gen_server:cast(Pid, {unsubscribe, Channel}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

sync_stop(Pid) ->
    gen_server:call(Pid, stop).

kickout_all() ->
    lists:foreach(fun({_Name, Pid, _Type, _Modules}) ->
        gen_server:call(Pid, kickout)
    end, supervisor:which_children(msg_server_sup)).

sync_msg(PlayerID, ClientPacketNumber) ->
    case ?GET_PID({connection, PlayerID}) of
        undefined -> ok;
        Pid -> gen_server:call(Pid, {sync_msg, ClientPacketNumber})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerID]) ->
    ?REG_PID({connection, PlayerID}),
    process_flag(trap_exit, true),
    Timer = erlang:send_after(?ACTIVITY_CHECK_DURATION, self(), active_check),
    {ok, #state{player_id = PlayerID,
                timer = Timer,
                channels = sets:new(),
                last_active_time = time_utils:now()}}.

%% When server trying to sync msg, but client closed tcp
handle_call({sync_msg, _ClientPacketNumber}, _From, State) 
  when State#state.socket =:= undefined ->
    {reply, fail, State};
handle_call({sync_msg, ClientPacketNumber}, _From, State) ->
    Queue = case get('__CACHE_MSG_QUEUE__') of
                undefined -> queue:new();
                Q -> Q
            end,
    Head = queue:peek(Queue),
    case Head =/= empty andalso element(1, Head) >= ClientPacketNumber of
        true ->
            lists:foldl(fun({PacketNumber, Packet}, Finded) ->
                if
                    Finded orelse PacketNumber > ClientPacketNumber ->
                        error_logger:info_msg("PlayerID: ~p, syncd_packet: ~p~n", 
                                              [State#state.player_id, PacketNumber]),
                        gen_tcp:send(State#state.socket, Packet),
                        true;
                    true ->
                        Finded
                end
            end, false, queue:to_list(Queue)),
            {reply, ok, State};
        false ->
            player:del_session(State#state.player_id),
            ensure_socket_closed(State),
            {stop, {shutdown, sync_msg_failed}, State}
    end;
handle_call(kickout, _From, State) ->
    ensure_socket_closed(State),
    {reply, ok, State#state{socket = undefined}};
handle_call(stop, _From, State) ->
    ensure_socket_closed(State),
    {stop, normal, State#state{socket = undefined}}.

handle_cast({controlling_process, Socket}, State) ->
    ensure_socket_closed(State),
    {noreply, State#state{socket = Socket}};
handle_cast({send_data, RequestId, Data}, State) ->
    error_logger:info_msg("PlayerID: ~p, RequestId: ~p, SendData: ~p~n", 
                          [State#state.player_id, RequestId, Data]),
    send_single_socket_data(State#state.socket, RequestId, Data),
    {noreply, State};
handle_cast({send_data, Data}, State) ->
    error_logger:info_msg("PlayerID: ~p, SendData: ~p~n", [State#state.player_id, Data]),
    send_single_socket_data(State#state.socket, 0, Data),
    {noreply, State};
handle_cast({send_multi_data, MultiData}, State) ->
    error_logger:info_msg("PlayerID: ~p, MultiData: ~p~n", 
                          [State#state.player_id, MultiData]),
    PackedData = [pack_response_data(RequestId, Data) || {RequestId, Data} <- MultiData],
    send_socket_data(State#state.socket, list_to_binary(PackedData)),
    {noreply, State};
handle_cast({subscribe, Channel}, State) ->
    List = proplists:get_value(gproc, gproc:info(self())),
    case proplists:is_defined({p, l, Channel}, List) of
        true -> ok;
        false -> ?SUBSCRIBE(Channel)
    end,
    {noreply, State#state{channels = sets:add_element(Channel, State#state.channels)}};
handle_cast({unsubscribe, Channel}, State) ->
    case proplists:get_value(gproc, gproc:info(self())) of
        [] -> ?UNSUBSCRIBE(Channel);
        _ -> ok
    end,
    List = proplists:get_value(gproc, gproc:info(self())),
    case proplists:is_defined({p, l, Channel}, List) of
        true -> ?UNSUBSCRIBE(Channel);
        false -> ok
    end,
    {noreply, State#state{channels = sets:del_element(Channel, State#state.channels)}};
handle_cast(stop, State) ->
    ensure_socket_closed(State),
    {stop, normal, State}.

handle_info({tcp, Socket, CipherData}, State=#state{player_id = PlayerID}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    RawData = secure:decrypt(?AES_KEY, ?AES_IVEC, CipherData),
    {RequestId, RequestContent} = utils_protocol:decode_integer(RawData),
    {RequestType, RequestBody} = utils_protocol:decode_short(RequestContent),
    case RequestType =:= ?RAW_REQUEST of
        false ->
            case routes:route(RequestType) of
                {error, Msg} ->
                    logger:info("Route Not Found: ~p~n", [Msg]),
                    send_fail_msg({fail, error_route_not_found}, RequestId, State),
                    {noreply, State#state{last_active_time = time_utils:now()}};
                RoutePath ->
                    {Params, _LeftData} = api_decoder:decode(RequestContent),
                    error_logger:info_msg("PlayerID: ~p, Request Path: ~p Parmas: ~p~n", 
                                          [PlayerID, RoutePath, Params]),
                    handle_request({RoutePath, Params, RequestId}, State)
            end;
        true ->
            Response = case middleware:handle(?RAW_REQUEST_HANDLER, [RequestBody]) of
                           {fail, handler_not_found} -> <<"handler_not_found">>;
                           {ok, Reply} -> Reply
                       end,
            EncodedRID = utils_protocol:encode_integer(RequestId),
            ProtocolID = utils_protocol:encode_short(?RAW_REQUEST),
            direct_send_socket_data(Socket, list_to_binary([EncodedRID, ProtocolID, Response])),
            {noreply, State#state{last_active_time = time_utils:now()}}
    end;
handle_info({gproc_msg, MsgType, Msg}, State=#state{player_id = PlayerID}) ->
    player:async_proxy(PlayerID, player_subscribe, handle, [MsgType, PlayerID, Msg]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_closed, PlayerID: ~p~n", [State#state.player_id]),
    ensure_socket_closed(State),
    {noreply, State#state{socket = undefined}};
handle_info({tcp_error, _Socket, _Msg}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_error, playerID: ~p~n", [State#state.player_id]),
    ensure_socket_closed(State),
    {noreply, State#state{socket = undefined}};
handle_info(active_check, State=#state{timer = Timer, last_active_time = ActiveAt}) ->
    erlang:cancel_timer(Timer),
    case time_utils:now() - ActiveAt >= ?EXPIRE_DURATION of
        true -> 
            error_logger:info_msg("DISCONNECT: tcp_timeout, PlayerID: ~p~n", 
                                  [State#state.player_id]),
            player:del_session(State#state.player_id),
            ensure_socket_closed(State),
            {stop, normal, State};
        false ->
            NewTimer = erlang:send_after(?ACTIVITY_CHECK_DURATION, self(), active_check),
            {noreply, State#state{timer = NewTimer}}
    end;
handle_info({stop, msg_queue_full}, State) ->
    case queue:len(get_msg_queue()) < ?MSG_QUEUE_LEN of
        true -> {noreply, State};
        false ->
            player:del_session(State#state.player_id),
            ensure_socket_closed(State),
            {stop, {shutdown, msg_queue_full}, State}
    end;
handle_info(Info, State) ->
    error_logger:info_msg("msg_server dropped handle_info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    error_logger:info_msg("msg_server terminate: ~p~n", [Reason]),
    ensure_socket_closed(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_request({Path = {sessions_controller, connect}, Params, RequestId}, State) ->
    del_msg_queue(),
    reset_packet_no(),
    reset_cache_msg_queue(),
    player:create_session(State#state.player_id, client_ip(State)),
    connect_common_logic(Path, Params, RequestId, State#state{channels = sets:new()});
handle_request({Path = {sessions_controller, reconnect}, Params, RequestId}, State) ->
    SessionId = proplists:get_value(session_id, Params),
    case player:get_session(State#state.player_id) of
        SessionId ->
            connect_common_logic(Path, Params, RequestId, State);
        _ ->
            send_fail_msg({fail, error_session_expired}, RequestId, State),
            error_logger:info_msg("DISCONNECT: session expired, can't reconnect!~n"),
            ensure_socket_closed(State),
            {noreply, State#state{socket = undefined}}
    end;
handle_request({Path, Params, RequestId}, State=#state{player_id = PlayerID}) ->
    case player:get_session(PlayerID) of
        undefined ->
            send_fail_msg({fail, error_session_expired}, RequestId, State),
            ensure_socket_closed(State),
            {noreply, State#state{socket = undefined}};
        _ ->
            track_online(PlayerID),
            player:request(PlayerID, Path, proplists_utils:values(Params), RequestId),
            {noreply, State#state{last_active_time = time_utils:now()}}
    end.

connect_common_logic(Path, Params, RequestId, State) ->
    Udid = proplists:get_value(udid, Params),
    {PlayerID, _Status} = player_factory:player_id(Udid),
    ets:insert(online_players, #online_players{uuid = PlayerID, 
                                               actived_at = time_utils:now()}),
    logger:info("Connected PlayerID: ~p~n", [PlayerID]),
    player:request(PlayerID, Path, proplists_utils:values(Params), RequestId),
    subscribe_channels(State),
    {noreply, State#state{last_active_time = time_utils:now()}}.

send_fail_msg(Msg, RequestId, State) ->
    send_single_socket_data(State#state.socket, RequestId, Msg).

send_single_socket_data(Socket, RequestId, Data) ->
    PureData = pack_response_data(RequestId, Data),
    send_socket_data(Socket, PureData).

pack_response_data(RequestId, Data) ->
    EncodedData = try encode_response(Data) of
                      EncodedResponse -> EncodedResponse
                  catch
                      Exception:Msg ->
                          exception:notify(Exception, Data, Msg),
                          api_encoder:encode(fail, {0, <<"Server Internal Error!">>})
                  end,
    list_to_binary([utils_protocol:encode_integer(RequestId), EncodedData]).

send_socket_data(Socket, PureData0) ->
    PureData = case byte_size(PureData0) > 300 of
                   true -> zlib:gzip(PureData0);
                   false -> PureData0
               end,
    PacketNumber = incr_packet_no(),
    NumberedData = list_to_binary([utils_protocol:encode_integer(PacketNumber), PureData]),
    CipherData = secure:encrypt(?AES_KEY, ?AES_IVEC, NumberedData),
    % case byte_size(PureData0) > 300000 of
    %     true ->
    %         exception:notify(large_response_error, [{gzip_pure_data, PureData}]);
    %     false ->
    %         ok
    % end,
    error_logger:info_msg("Response DataSize: ~p, PureDataSize: ~p, GzipSize: ~p~n", 
                          [byte_size(CipherData), 
                           byte_size(PureData0),
                           byte_size(NumberedData)]),
    Queue = queue_msg(PacketNumber, CipherData),
    if
        Socket =/= undefined ->
            flush_msg_queue(Socket, Queue);
        true ->
            case queue:len(Queue) < ?MSG_QUEUE_LEN of
                true -> ok;
                false -> self() ! {stop, msg_queue_full}
            end
    end.

direct_send_socket_data(Socket, PureData0) ->
    PureData = case byte_size(PureData0) > 300 of
                   true -> zlib:gzip(PureData0);
                   false -> PureData0
               end,
    NumberedData = list_to_binary([utils_protocol:encode_integer(-1000), PureData]),
    CipherData = secure:encrypt(?AES_KEY, ?AES_IVEC, NumberedData),
    error_logger:info_msg("Response DataSize: ~p, PureDataSize: ~p, GzipSize: ~p~n", 
                          [byte_size(CipherData), 
                           byte_size(PureData0),
                           byte_size(NumberedData)]),
    if
        Socket =/= undefined ->
            gen_tcp:send(Socket, CipherData);
        true -> ok
    end.

reset_packet_no() ->
    Key = {?MODULE, packet_number},
    put(Key, 0).

incr_packet_no() ->
    Key = {?MODULE, packet_number},
    case get(Key) of
        undefined ->
            put(Key, 1),
            1;
        N ->
            Number = (N + 1) rem 2147483647,
            put(Key, Number),
            Number
    end.

flush_msg_queue(Socket, Queue) ->
    case queue:peek(Queue) of
        empty -> ok;
        {value, {PacketNumber, Data}} ->
            error_logger:info_msg("send packet: ~p~n", [PacketNumber]),
            Result = try do_send_data(Socket, Data) of
                Status -> Status
            catch
                _Exception:_Msg ->
                    {error, exception}
            end,
            case Result of
                ok -> 
                    cache_msg(PacketNumber, Data),
                    flush_msg_queue(Socket, pop_msg_queue(Queue));
                _ -> ok
            end
    end.

do_send_data(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
        ok -> ok;
        {error, Reason} ->
            error_logger:info_msg("Send socket data failed: ~p~n", [Reason]),
            {error, Reason}
    end.

queue_msg(PacketNumber, Packet) ->
    Queue = get_msg_queue(),
    NQueue = queue:in({PacketNumber, Packet}, Queue),
    put('__MSG_QUEUE__', NQueue),
    NQueue.

pop_msg_queue(Queue) ->
    NQueue = queue:drop(Queue),
    put('__MSG_QUEUE__', NQueue),
    NQueue.

get_msg_queue() ->
    case get('__MSG_QUEUE__') of
        undefined -> queue:new();
        Q -> Q
    end.

del_msg_queue() ->
    erase('__MSG_QUEUE__'),
    queue:new().

cache_msg(PacketNumber, Packet) ->
    Queue = case get('__CACHE_MSG_QUEUE__') of
                undefined -> queue:new();
                Q -> Q
            end,
    Queue1 = queue:in({PacketNumber, Packet}, Queue),
    Queue2 = case queue:len(Queue1) > ?CACHE_QUEUE_MAX_LEN of
                 true -> queue:drop(Queue1);
                 false -> Queue1
             end,
    put('__CACHE_MSG_QUEUE__', Queue2).

reset_cache_msg_queue() ->
    erase('__CACHE_MSG_QUEUE__').

encode_response(Response) ->
    case Response of
        {fail, ErrorAtom} ->
            ErrorString = atom_to_binary(ErrorAtom, utf8),
            case game_numerical:find(config_error_msgs, ErrorString) of
                undefined ->
                    api_encoder:encode(fail, {0, ErrorString});
                Conf ->
                    api_encoder:encode(fail, {Conf#config_error_msgs.no, <<"">>})
            end;
        {fail, ErrorAtom, Msg} ->
            ErrorString = atom_to_binary(ErrorAtom, utf8),
            case game_numerical:find(config_error_msgs, ErrorString) of
                undefined ->
                    api_encoder:encode(fail, {0, ErrorString});
                Conf ->
                    api_encoder:encode(fail, {Conf#config_error_msgs.no, Msg})
            end;
        {Protocol, Msg} when is_tuple(Msg) ->
            api_encoder:encode(Protocol, Msg);
        {Protocol, Msg} when is_list(Msg) orelse is_binary(Msg) ->
            api_encoder:encode(Protocol, {Msg});
        {_Protocol, Msg} ->
            error_logger:info_msg("Response Msg type error: ~p", [Msg]),
            erlang:error("Encode response error, Invalid Msg type!")
    end.

track_online(PlayerID) ->
    case ets:lookup(online_players, PlayerID) of
        [] -> ets:insert(online_players, #online_players{uuid = PlayerID, 
                                                         actived_at = time_utils:now()});
        _  -> ets:update_element(online_players, PlayerID, 
                                 {#online_players.actived_at, time_utils:now()})
    end.

track_offline(PlayerID) ->
    ets:delete(online_players, PlayerID).

ensure_socket_closed(State) ->
    case State#state.socket of
        undefined -> ok;
        Socket -> 
            track_offline(State#state.player_id),
            unsubscribe_channels(State),
            gen_tcp:close(Socket)
            % player:on_tcp_closed(State#state.player_id)
    end.

subscribe_channels(State) ->
    lists:foreach(fun(Channel) ->
        List = proplists:get_value(gproc, gproc:info(self())),
        case proplists:is_defined({p, l, Channel}, List) of
            true -> ok;
            false -> ?SUBSCRIBE(Channel)
        end
    end, sets:to_list(State#state.channels)).

%% FIXME 临时测试
unsubscribe_channels(State) ->
    % lists:foreach(fun(Channel) ->
    %     List = proplists:get_value(gproc, gproc:info(self())),
    %     case proplists:is_defined({p, l, Channel}, List) of
    %         true -> ?UNSUBSCRIBE(Channel);
    %         false -> ok
    %     end
    % end, sets:to_list(State#state.channels)).
    ok.

client_ip(State) ->
    if
        State#state.socket =:= undefined -> undefined;
        true ->
            case inet:peername(State#state.socket) of
                {ok, {Address, _Port}} -> inet_parse:ntoa(Address);
                {error, _} -> undefined
            end
    end.
