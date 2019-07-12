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


-module(api_connection).

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4, stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(protocol, {ref, socket, transport}).

-include("../app/include/game_const.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport], Opts).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Ref, Socket, Transport]) ->
    {ok, #protocol{ref = Ref, socket = Socket, transport = Transport}, 0}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, State=#protocol{transport = Transport, socket = Socket}) ->
    {ok, {Address, _Port}} = ranch_tcp:peername(Socket),
    error_logger:info_msg("New Connection: ~p~n", [ranch_tcp:peername(Socket)]),
    case lists:member(Address, ?API_IPS) of
        true ->
            ok = ranch:accept_ack(State#protocol.ref),
            ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),
            {noreply, State};
        false ->
            error_logger:info_msg("Client has no permission!"),
            ranch_tcp:close(Socket),
            {stop, normal, State}
    end;
handle_info({tcp, Socket, Data}, State=#protocol{transport = Transport}) ->
    error_logger:info_msg("Received Data: ~p~n", [Data]),
    ok = Transport:setopts(Socket, [{active, once}]),
    Params = jsx:decode(Data),
    Path = proplists:get_value(<<"_path">>, Params),
    Response = apis_controller:handle(Path, Params),
    error_logger:info_msg("Response: ~p~n", [Response]),
    ResJson = jsx:encode(Response),
    Transport:send(Socket, list_to_binary([ResJson, <<"\n">>])),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_closed~n"),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _Msg}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_error~n"),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
