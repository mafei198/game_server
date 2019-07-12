-module(auth_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_socket/1,
         set_enable/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {enable = false}).

-include("../app/include/secure.hrl").
-include("include/gproc_macros.hrl").
-include ("include/common_const.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_socket(Socket) ->
    gen_tcp:controlling_process(Socket, whereis(auth_server)),
    gen_server:cast(?SERVER, {add_socket, Socket}).

set_enable(Enabled) ->
    gen_server:call(?SERVER, {set_enable, Enabled}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({set_enable, Enabled}, _From, State) ->
    {reply, ok, State#state{enable = Enabled}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_socket, Socket}, State) ->
    ok = inet:setopts(Socket, [{active, once}, {packet, 4}]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, CipherData}, State) ->
    if
        State#state.enable ->
            try handle_auth(Socket, CipherData) of
                _Resp -> ok
            catch
                Exception:Msg ->
                    error_logger:info_msg("TCP CLOSED! Auth Exception: ~p, ~p~n", 
                                          [Exception, Msg]),
                    gen_tcp:close(Socket)
            end;
        true ->
            error_logger:info_msg("TCP CLOSED! Server not open!~n"),
            gen_tcp:close(Socket)
    end,
    {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
    error_logger:info_msg("DISCONNECT! tcp_closed: ~p~n", [inet:peername(Socket)]),
    gen_tcp:close(Socket),
    {noreply, State};
handle_info({tcp_error, Socket, Msg}, State) ->
    error_logger:info_msg("DISCONNECT! tcp_error: ~p, Socket: ~p~n", 
                          [Msg, inet:peername(Socket)]),
    gen_tcp:close(Socket),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_auth(Socket, CipherData) ->
    RawData = secure:decrypt(?AES_KEY, ?AES_IVEC, CipherData),
    {_RequestId, RequestContent} = utils_protocol:decode_integer(RawData),
    {RequestType, RequestBody} = utils_protocol:decode_short(RequestContent),
    Route = routes:route(RequestType),
    if
        RequestType =:= ?RAW_REQUEST orelse
        Route =:= {sessions_controller, connect} orelse
        Route =:= {sessions_controller, reconnect} ->
            Udid = if
                       RequestType =:= ?RAW_REQUEST ->
                           {Body, _} = utils_protocol:decode_string(RequestBody),
                           Params = jsx:decode(Body),
                           proplists:get_value(<<"udid">>, Params);
                       true ->
                           {Params, _LeftData} = api_decoder:decode(RequestContent),
                           proplists:get_value(udid, Params)
                   end,
            if
                Udid =:= undefined -> 
                    gen_tcp:close(Socket);
                true ->
                    {PlayerID, _} = player_factory:player_id(Udid),
                    ConnPid = case ?GET_PID({connection, PlayerID}) of
                                  undefined ->
                                      {ok, Pid} = msg_server_sup:start_child([PlayerID]),
                                      Pid;
                                  Pid -> Pid
                              end,
                    msg_server:controlling_process(ConnPid, Socket),
                    ConnPid ! {tcp, Socket, CipherData}
            end;
        true -> 
            error_logger:info_msg("TCP CLOSED! Auth failed: ~p~n", [Route]),
            gen_tcp:close(Socket)
    end.
