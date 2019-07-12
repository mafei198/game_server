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
%%%        Manage timertask!
%%% @end
%%% Created :  一  3 24 18:20:18 2014 by Me

-module(timertask).

-behaviour(gen_server).

%% API
-export([start_link/0, 
         stop_check/0,
         start_check_timer/0,
         sync_add/3, 
         sync_cancel/1, 
         sync_update/3,
         add/3, 
         cancel/1, 
         update/3, 
         lookup/1]).

%% Internal API
-export([add_timer/3, cancel_timer/1, update_timer/3, get_timer/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(KEY, "timertask:ordered_set").
-define(QUEUE, {tmp, failed_queue}).
-define(MAX_RETRY, 5).

-record(state, {queue, working}).

-define(TABLE, game_server_timertask).
-define(ORDER_TABLE, game_server_ordered_timertask).


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_check_timer() ->
    gen_server:call(?SERVER, start_check_timer).

stop_check() ->
    gen_server:call(?SERVER, stop_check, infinity).

add(Key, RunAt, MFA) ->
    gen_server:cast(?SERVER, {add_timer, [Key, RunAt, MFA]}).

cancel(Key) ->
    gen_server:cast(?SERVER, {cancel_timer, [Key]}).

update(Key, RunAt, MFA) ->
    gen_server:cast(?SERVER, {update_timer, [Key, RunAt, MFA]}).

sync_add(Key, RunAt, MFA) ->
    gen_server:call(?SERVER, {add_timer, [Key, RunAt, MFA]}).

sync_cancel(Key) ->
    gen_server:call(?SERVER, {cancel_timer, [Key]}).

sync_update(Key, RunAt, MFA) ->
    gen_server:call(?SERVER, {update_timer, [Key, RunAt, MFA]}).

lookup(Key) ->
    gen_server:call(?SERVER, {get_timer, [Key]}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    [Host, Port, DB] = get_redis_config(),
    {ok, Redis} = eredis:start_link(Host, Port, DB),
    put(redis, Redis),
    {ok, #state{queue = queue:new(), working = true}}.

handle_call(start_check_timer, _From, State) ->
    timer:send_after(1000, {start_timertask}),
    {reply, ok, State};
handle_call(stop_check, _From, State) ->
    {reply, ok, State#state{working = false}};
handle_call({Cmd, Args}, From, State=#state{queue=Queue}) ->
    Queue1 = queue:in({Cmd, Args, From}, Queue),
    Queue2 = execute_queue(Queue1),
    {noreply, State#state{queue = Queue2}}.

handle_cast({Cmd, Args}, State=#state{queue=Queue}) ->
    Queue1 = queue:in({Cmd, Args, undefined}, Queue),
    Queue2 = execute_queue(Queue1),
    {noreply, State#state{queue = Queue2}}.

handle_info({start_timertask}, State) ->
    try_restart_timer(),
    {noreply, State};
handle_info({timertask}, State=#state{working = Working}) ->
    if
        Working ->
            erase(current_timer),
            case first_timer() of
                undefined -> ok;
                {Key, RunAt, MFA} ->
                    case RunAt =< time_utils:now() of
                        true -> execute_timer(Key, MFA);
                        false -> try_restart_timer()
                    end
            end;
        true ->
            error_logger:warning_msg("timertask stopped check~n")
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
execute_timer(Key, MFA) ->
    try dispatch_to_worker(MFA) of
        _ -> 
            cancel_timer(Key)
    catch
        Type:Msg ->
            cancel_timer(Key),
            exception:notify(Type, {timertask_exception, Key, MFA}, Msg)
    end.

add_timer(Key, RunAt, MFA) ->
    transaction(fun() ->
        redis_cmd(["zadd", ?KEY, RunAt, Key]),
        redis_cmd(["set", mfa_key(Key), encode(MFA)])
    end),
    try_restart_timer().

cancel_timer(Key) ->
    transaction(fun() ->
        redis_cmd(["zrem", ?KEY, Key]),
        redis_cmd(["del", mfa_key(Key)])
    end),
    try_restart_timer().

update_timer(Key, RunAt, MFA) ->
    transaction(fun() ->
        redis_cmd(["zadd", ?KEY, RunAt, Key]),
        redis_cmd(["set", mfa_key(Key), encode(MFA)])
    end),
    try_restart_timer().

dispatch_to_worker(MFA) ->
    poolboy:transaction(timertask_worker_pool, fun(Worker) ->
        gen_server:cast(Worker, {timertask, MFA})
    end).

first_timer() ->
    case redis_cmd(["zrange", ?KEY, 0, 0, "withscores"]) of
        {ok, []} -> undefined;
        {ok, [Key, RunAt]} -> 
            {ok, MFA} = redis_cmd(["get", mfa_key(Key)]),
            {Key, binary_to_integer(RunAt), decode(MFA)}
    end.

get_timer(Key) ->
    case redis_cmd(["zscore", ?KEY, Key]) of
        {ok, undefined} -> undefined;
        {ok, RunAt} -> 
            {ok, MFA} = redis_cmd(["get", mfa_key(Key)]),
            {Key, binary_to_integer(RunAt), decode(MFA)}
    end.

start_timer({_Key, RunAt, _MFA}) ->
    AfterTime = lists:max([(RunAt - current_time()) * 1000, 0]),
    Timer = erlang:send_after(AfterTime, ?SERVER, {timertask}),
    put(current_timer, {RunAt, Timer}).

try_restart_timer() ->
    case first_timer() of
        undefined -> ok;
        FirstTimer ->
            case get(current_timer) of
                undefined ->
                    start_timer(FirstTimer);
                {RunAt, Timer} ->
                    FirstRunAt = element(2, FirstTimer),
                    if
                        FirstRunAt =/= RunAt ->
                            erlang:cancel_timer(Timer),
                            start_timer(FirstTimer);
                        true -> ok
                    end
            end
    end.

current_time() ->
    Datetime = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Datetime) - 62167219200.

encode(Data) ->
    Value = term_to_binary(Data),
    base64:encode(Value).

decode(Value) ->
    Data = base64:decode(Value),
    binary_to_term(Data).

transaction(Fun) ->
    Redis = get(redis),
    try
        {ok, <<"OK">>} = eredis:q(Redis, ["MULTI"]),
        Fun(),
        {ok, V} = eredis:q(Redis, ["EXEC"]),
        {ok, V}
    catch Klass:Reason ->
            {ok, <<"OK">>} = eredis:q(Redis, ["DISCARD"]),
            error(Reason, Klass)
    end.


redis_cmd(List) ->
    Redis = get(redis),
    {ok, V} = eredis:q(Redis, List),
    {ok, V}.

mfa_key(Key) when is_atom(Key) ->
   "timertask:mfa_set:" ++ atom_to_list(Key);
mfa_key(Key) when is_list(Key) ->
   "timertask:mfa_set:" ++ Key;
mfa_key(Key) when is_binary(Key) ->
   "timertask:mfa_set:" ++ binary_to_list(Key).

%% Private
get_redis_config() ->
    Host = configuration:get(gs_config, redis_host, "127.0.0.1"),
    Port = configuration:get(gs_config, redis_port, 6379),
    DB   = configuration:get(gs_config, redis_db, 0),
    [Host, Port, DB].

execute_queue(Queue) ->
    case queue:peek(Queue) of
        {value, {Cmd, Args, From}} ->
            % error_logger:info_msg("Cmd: ~p, Args: ~p, From: ~p~n", [Cmd, Args, From]),
            try erlang:apply(?MODULE, Cmd, Args) of
                Result ->
                    try_reply(From, Result),
                    NewQueue = queue:drop(Queue),
                    case queue:is_empty(NewQueue) of
                        false ->
                            execute_queue(NewQueue);
                        true ->
                            NewQueue
                    end
            catch 
                Type:Msg ->
                    spawn(fun() ->
                        exception:notify(Type, {timertask_failed, Cmd, Args}, Msg)
                    end),
                    try_reply(From, {error, {Type, Msg}}),
                    queue:drop(Queue)
            end;
        empty -> Queue
    end.

try_reply(undefined, _Reply) -> ok;
try_reply(From, Reply) -> gen_server:reply(From, Reply).
