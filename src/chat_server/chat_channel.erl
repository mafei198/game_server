%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014-2024
%%% Savin Max <mafei.198@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%
%%% @doc
%%%        Chat Channel.
%%% @end
%%% Created :  三  3 12 17:29:50 2014 by Savin Max

-module(chat_channel).

-behaviour(gen_server).

%% API
-export([start_link/2,
         clear/1,
         sync_history/2,
         history/4,
         history/5,
         broadcast/2,
         send_data/3, %% custom data
         join/3,
         leave/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/gproc_macros.hrl").
-include("../app/include/custom_record.hrl").
-include("../app/include/game_const.hrl").

-record(state, {channel, 
                queue_name, 
                current_amount, 
                max_cache_amount}).

-define(TAB, ?MODULE).
-define(CHANNEL_EXPIRE_SECS, 604800). % 7 days

-define(SUBSCRIBERS, chat_server_subscribers).
-define(TRANS, chat_server_trans).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id, MaxCacheAmount) ->
    gen_server:start_link(?MODULE, [Id, MaxCacheAmount], []).

join(PlayerID, Locale, Channel) ->
    leave(PlayerID, Channel),
    ets:insert(?SUBSCRIBERS, {{Channel, players}, {PlayerID, Locale}}).

leave(PlayerID, Channel) ->
    ets:match_delete(?SUBSCRIBERS, {{Channel, players}, {PlayerID, '_'}}).

clear(Channel) ->
    gen_server:call(channel_pid(Channel), clear_history).

sync_history(Channel, Amount) ->
    gen_server:call(channel_pid(Channel), {sync_history, Amount, 0}).

history(Channel, PlayerID, Locale, Amount) ->
    gen_server:cast(channel_pid(Channel), {history, PlayerID, Locale, Amount, 0}).

history(Channel, PlayerID, Locale, Amount, ReceivedAmount) ->
    gen_server:cast(channel_pid(Channel), {history, PlayerID, Locale, Amount, ReceivedAmount}).

broadcast(Channel, Msg) ->
    gen_server:cast(channel_pid(Channel), {broadcast, Msg}).

send_data(Channel, MsgType, Msg) ->
    gen_server:cast(channel_pid(Channel), {send_data, MsgType, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Channel, MaxCacheAmount]) ->
    ?REG_PID({chat_channel, Channel}),
    QueueName = misc_utils:encode_term({Channel, 1.1}),
    {ok, #state{channel=Channel,
                queue_name = QueueName,
                current_amount = get_queue_len(QueueName),
                max_cache_amount = MaxCacheAmount}}.

handle_call({sync_history, Amount, ReceivedAmount}, _From, State) ->
    {reply, sync_history_msg(Amount, ReceivedAmount, State), State};
handle_call(clear_history, _From, State) ->
    eredis_pool:q(redis, ["del", State#state.queue_name]),
    {reply, ok, State#state{current_amount = 0}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({broadcast, Msg}, State=#state{channel=Channel}) ->
    NMsg = if
               Msg#chat_msg.category =:= 0 ->
                   detect_locale(Msg);
               true -> Msg
           end,
    do_broadcast(Channel, NMsg),
    NState = add_msg(NMsg, State),
    {noreply, NState};
handle_cast({send_data, MsgType, Msg}, State=#state{channel=Channel}) ->
    Subscribers = ets:lookup(?SUBSCRIBERS, {Channel, players}),
    lists:foreach(fun({_, {PlayerID, _Locale}}) ->
        case player:conn_alive(PlayerID) of
            true ->
                player:send_data(PlayerID, {MsgType, Msg});
            false ->
                ok
        end
    end, Subscribers),
    {noreply, State};
handle_cast({history, PlayerID, Locale, Amount, ReceivedAmount}, State) ->
    history_msg(PlayerID, Locale, Amount, ReceivedAmount, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel=Channel}) ->
    ?UNREG({chat_channel, Channel}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_msg(Msg, State) ->
    Queue = State#state.queue_name,
    {ok, _} = eredis_pool:q(redis, ["RPUSH", Queue, misc_utils:encode_term(Msg)]),
    {ok, _} = eredis_pool:q(redis, ["EXPIRE", Queue, ?CHANNEL_EXPIRE_SECS]),
    Max = State#state.max_cache_amount,
    case State#state.current_amount > Max of
        false -> 
            State#state{current_amount = State#state.current_amount + 1};
        true ->
            From = trunc(Max/2),
            {ok, _} = eredis_pool:q(redis, ["LTRIM", Queue, From, -1]),
            State#state{current_amount = get_queue_len(Queue)}
    end.

sync_history_msg(Amount, ReceivedAmount, State) ->
    Queue = State#state.queue_name,
    To = State#state.current_amount - ReceivedAmount - 1,
    From = lists:max([0, To - Amount]),
    case From =< To of
        false -> [];
        true ->
            {ok, Msgs} = eredis_pool:q(redis, ["LRANGE", Queue, From, To]),
            FinalMsgs = lists:foldl(fun(Msg, Acc) ->
                TupleMsg = misc_utils:decode_term(Msg),
                [info(TupleMsg)|Acc]
            end, [], Msgs),
            lists:reverse(FinalMsgs)
    end.

history_msg(PlayerID, Locale, Amount, ReceivedAmount, State) ->
    Queue = State#state.queue_name,
    To = State#state.current_amount - ReceivedAmount - 1,
    From = lists:max([0, To - Amount]),
    case From =< To of
        false -> [];
        true ->
            {ok, Msgs} = eredis_pool:q(redis, ["LRANGE", Queue, From, To]),
            FinalMsgs = lists:foldl(fun(Msg, Acc) ->
                TupleMsg = misc_utils:decode_term(Msg),
                [info(translate(TupleMsg, Locale))|Acc]
            end, [], Msgs),
            player:send_data(PlayerID, {chat_msgs, lists:reverse(FinalMsgs)})
    end.

channel_pid(Channel) ->
    case ?GET_PID({chat_channel, Channel}) of
        undefined -> 
            {ok, Pid} = chat_server:create_channel(Channel),
            Pid;
        ChannelPid -> ChannelPid
    end.

get_queue_len(QueueName) ->
    {ok, Len} = eredis_pool:q(redis, ["LLEN", QueueName]),
    binary_to_integer(Len).

detect_locale(Msg) ->
    case configuration:get(gs_config, enable_translate, true) of
        true ->
            Locale = api_helper:detect_locale(Msg#chat_msg.content),
            Msg#chat_msg{locale = Locale};
        false ->
            Msg
    end.

do_broadcast(Channel, Msg) ->
    Subscribers = ets:lookup(?SUBSCRIBERS, {Channel, players}),
    lists:foreach(fun({_, {PlayerID, Locale}}) ->
        case player:conn_alive(PlayerID) of
            true ->
                TransMsg = translate(Msg, Locale),
                player:send_data(PlayerID, {chat_msg, info(TransMsg)});
            false ->
                leave(PlayerID, Channel)
        end
    end, Subscribers).

-define(LOCALE_MAPS, [{<<"cn">>, <<"zh-CN">>}]).

translate(Msg, _TargetLocale0) when Msg#chat_msg.category =:= ?MSG_REPORT ->
    Msg;
translate(Msg, TargetLocale0) ->
    TargetLocale = if
                       TargetLocale0 =:= <<"cn">> -> <<"zh-CN">>;
                       true -> TargetLocale0
                   end,
    case configuration:get(gs_config, enable_translate, true) of
        true ->
            if
                Msg#chat_msg.locale =:= TargetLocale ->
                    Msg;
                true ->
                    Key = {erlang:md5(Msg#chat_msg.content), TargetLocale},
                    Extra = case ets:lookup(?TRANS, Key) of
                                [{_, Trans}] -> Trans;
                                [] ->
                                    Trans = api_helper:translate(Msg#chat_msg.content, 
                                                                 Msg#chat_msg.locale,
                                                                 TargetLocale),
                                    logger:info("Content: ~p, Locale: ~p, TargetLocale: ~p, Trans: ~p~n", 
                                                [Msg#chat_msg.content, Msg#chat_msg.locale,
                                                 TargetLocale, Trans]),
                                    ets:insert(?TRANS, {Key, Trans}),
                                    Trans
                            end,
                    Msg#chat_msg{extra = Extra}
            end;
        false ->
            Msg
    end.

info(Msg) ->
    {Msg#chat_msg.uuid,
     Msg#chat_msg.name,
     Msg#chat_msg.user_id,
     Msg#chat_msg.user_level,
     Msg#chat_msg.alliance_name,
     Msg#chat_msg.alliance_pos,
     Msg#chat_msg.channel,
     Msg#chat_msg.content,
     Msg#chat_msg.created_at,
     Msg#chat_msg.category,
     Msg#chat_msg.extra,
     Msg#chat_msg.locale}.
