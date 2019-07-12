-module(game_log).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add/1,
         sync_persist/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-define(SLEEP_TIME, 1000).
-define(BATCH_AMOUNT, 50).

-record(state, {timer}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Log) ->
    ets:insert(?TAB, {game_counter:incr_tmp({?MODULE, tracker}), Log}).

sync_persist() ->
    gen_server:call(?SERVER, {sync_persist}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    NewTimer = erlang:send_after(?SLEEP_TIME, self(), circulation_persist),
    {ok, #state{timer = NewTimer}}.

handle_call({sync_persist}, _From, State=#state{timer = Timer}) ->
    erlang:cancel_timer(Timer),
    NewTimer = batch_persist(),
    {reply, true, State#state{timer = NewTimer}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(circulation_persist, State=#state{timer = Timer}) ->
    erlang:cancel_timer(Timer),
    NewTimer = batch_persist(),
    {noreply, State#state{timer = NewTimer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
batch_persist() ->
    try do_batch_persist() of
        R -> R
    catch
        Type:Msg ->
            exception:notify(Type, [<<"game_log persist failed">>], Msg),
            erlang:send_after(?SLEEP_TIME, self(), circulation_persist)
    end.

do_batch_persist() ->
    Size = ets:info(?TAB, size),
    if
        Size =:= 0 orelse Size =:= undefined ->
            erlang:send_after(?SLEEP_TIME, self(), circulation_persist);
        true -> 
            batch_create(lists:min([Size, ?BATCH_AMOUNT]))
    end.

batch_create(N) ->
    Logs = misc_utils:upto(fun(_, Acc) ->
        Key = ets:first(?TAB),
        [{_, Log}] = ets:lookup(?TAB, Key),
        ets:delete(?TAB, Key),
        [Log|Acc]
    end, [], 1, N),
    case Logs of
        [] -> ok;
        _ ->
            Sql = sql_generator:gen_batch_create_sql(Logs),
            do_execute(Sql)
    end,
    batch_persist().

do_execute(Sql) ->
    try db:direct_execute(Sql) of
        Result -> Result
    catch
        Type:Msg ->
            exception:notify(Type, [<<"game_log persist failed">>], Msg),
            timer:sleep(2000),
            do_execute(Sql)
    end.
