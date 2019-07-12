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


-module(player_factory).

-behaviour(gen_server).

%% API
-export([start_link/0,
         shutdown_players/0,
         player_id/1,
         clean_udid_mapping/1,
         start_player/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TAB, ?MODULE).
-define(WORKING, 0).
-define(SHUTDOWN, 1).
-define(TERMINATE, 2).


-include("player_macros.hrl").
-record(ws_globals, {key, value}).

-record(state, {status, timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

player_id(Udid) ->
    case game_counter:get({proxy, unique_device_id, Udid}) of
        undefined ->
            case game_counter:get({unique_device_id, Udid}) of
                undefined -> 
                    case gen_server:call(?MODULE, {player_id, Udid}) of
                        {ok, Reply} -> Reply;
                        {exception, Type, Msg} ->
                            error({Type, Msg}, [Udid])
                    end;
                {Id, Status} -> {Id, Status}
            end;
        IdPacket -> IdPacket
    end.

clean_udid_mapping(Udid) ->
    case game_counter:get({proxy, unique_device_id, Udid}) of
        undefined ->
            case game_counter:get({unique_device_id, Udid}) of
                undefined -> ok;
                _IdPacket -> game_counter:del({unique_device_id, Udid})
            end;
        _IdPacket -> 
            game_counter:del({proxy, unique_device_id, Udid})
    end.

-spec(start_player(PlayerID::binary()) -> {ok, pid()}).
start_player(PlayerID) ->
    gen_server:call(?MODULE, {start_player, PlayerID}).

shutdown_players() ->
    gen_server:cast(?MODULE, shutdown_players).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{status=?WORKING}}.

handle_call({player_id, Udid}, _From, State) ->
    Result = try factory_player_id(Udid) of
                 R -> {ok, R}
             catch
                 Type:Msg ->
                     {exception, Type, Msg}
             end,
    {reply, Result, State};
handle_call({start_player, PlayerID}, _From, State=#state{status=Status}) ->
    case Status =:= ?WORKING of
        true ->
            Result = case ?GET_PID({player, PlayerID}) of
                undefined ->
                    % logger:info("player_factory will start player from player_sup~n"),
                    player_sup:start_child([PlayerID]);
                Pid ->
                    % logger:info("player_factory get player_pid from gproc~n"),
                    {ok, Pid}
            end,
            {reply, Result, State};
        false ->
            % logger:info("player_factory, wrong status~n"),
            {reply, undefined, State}
    end.

handle_cast(shutdown_players, State) ->
    Children = supervisor:which_children(player_sup),
    start_shutdown_children(Children),
    {noreply, State#state{status=?SHUTDOWN}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_finish_shutdown, State=#state{timer=Timer}) ->
    Children = supervisor:which_children(player_sup),
    if
        length(Children) =:= 0 ->
            erlang:cancel_timer(Timer),
            notify_shutdown_finished(),
            {noreply, State};
        true ->
            erlang:cancel_timer(Timer),
            NewTimer = erlang:send_after(1000, self(), check_finish_shutdown),
            {noreply, State#state{timer=NewTimer}}
    end;
handle_info({finished_shutdown, _From}, State=#state{status=?TERMINATE}) ->
    {noreply, State};
handle_info({finished_shutdown, _From}, State=#state{status=?SHUTDOWN}) ->
    NewState = case shutdown_next() of
                   shutdown_finished ->
                       Timer = case supervisor:which_children(player_sup) of
                                   [] -> notify_shutdown_finished();
                                   _ -> erlang:send_after(1000, self(), check_finish_shutdown)
                               end,
                       State#state{status=?TERMINATE, timer=Timer};
                   _ -> State
               end,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
shutdown_next() ->
    case get({player, children}) of
        [] -> shutdown_finished;
        [Child|T] ->
            put({player, children}, T),
            shutdown(Child)
    end.

start_shutdown_children([]) -> notify_shutdown_finished();
start_shutdown_children(Children) ->
    % CpuAmount = erlang:system_info(schedulers_online),
    % shutdown(Children, CpuAmount * 2).
    shutdown(Children, 1).

shutdown([], _Amount) ->
    put({player, children}, []);
shutdown(Children, 0) ->
    put({player, children}, Children);
shutdown([Child|Children], Amount) ->
    shutdown(Child),
    shutdown(Children, Amount - 1).

shutdown(_Child={_Name, Pid, _Type, _Modules}) ->
    Pid ! {shutdown, self()}.

notify_shutdown_finished() ->
    game_server ! {finished_shutdown_players, self()}.

factory_player_id(Udid) ->
    case game_counter:get({unique_device_id, Udid}) of
        undefined -> 
            IdPacket = {uuid_factory:gen(), unregistered},
            shared_data:write(#ws_globals{key = {unique_device_id, Udid}, 
                                          value = IdPacket}),
            IdPacket;
        IdPacket -> 
            IdPacket
    end.
