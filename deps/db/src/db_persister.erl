-module(db_persister).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add/3,
         sync_persist/0,
         is_persist_finished/1,
         ensure_persisted/1]).

%% API for hot fix sql error
-export([find/1,
         update/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SLEEP_TIME, 1000).

-define(TAB, ?MODULE).
-define(TAB_COUNTER, db_counter).

-record(state, {timer}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

find(Key) ->
    case ets:lookup(?TAB, Key) of
        [] -> undefined;
        [V] -> V
    end.

update(Key, {Counter, PlayerID, Version, Sql}) ->
    case find(Key) of
        undefined -> fail_record_not_found;
        {Counter, PlayerID, Version, _OldSql} ->
            ets:insert(?TAB, {Counter, PlayerID, Version, Sql})
    end.

add(PlayerID, Version, Sql) ->
    ets:update_counter(?TAB_COUNTER, PlayerID, {2, 1}, {PlayerID, 0}),
    Counter = ets:update_counter(?TAB_COUNTER, persist_queue, {2, 1}, {persist_queue, 0}),
    % logger:info("Counter: ~p~n", [Counter]),
    true = ets:insert(?TAB, {Counter, PlayerID, Version, Sql}).

is_persist_finished(PlayerID) ->
    case ets:lookup(?TAB_COUNTER, PlayerID) of
        [] -> true;
        [{PlayerID, Counter}] ->
            if
                Counter > 0 -> false;
                Counter =:= 0 -> true
            end
    end.

ensure_persisted(PlayerID) ->
    try_ensure_persisted(PlayerID, 10).

try_ensure_persisted(_PlayerID, 0) -> false;
try_ensure_persisted(PlayerID, N) ->
    case ets:lookup(?TAB_COUNTER, PlayerID) of
        [] -> true;
        [{PlayerID, Counter}] ->
            logger:info("ensure_persisted: ~p, ~p~n", [PlayerID, Counter]),
            if
                Counter > 0 -> 
                    timer:sleep(3000), %% 隔3秒再试
                    try_ensure_persisted(PlayerID, N - 1);
                Counter =:= 0 -> true
            end
    end.

sync_persist() ->
    gen_server:call(?SERVER, {sync_persist}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    NewTimer = erlang:send_after(?SLEEP_TIME, self(), circulation_persist),
    {ok, #state{timer = NewTimer}}.

handle_call({sync_persist}, _From, State) ->
    persist(),
    Size = ets:info(?TAB, size),
    Reply = if
                Size =:= 0 orelse Size =:= undefined -> true;
                true -> false
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(circulation_persist, State=#state{timer = Timer}) ->
    erlang:cancel_timer(Timer),
    NewTimer = try persist() of
                   Result -> Result
               catch
                   Type:Msg ->
                        exception:notify(Type, [<<"PlayerID">>, get(player_id)], Msg),
                        erlang:send_after(?SLEEP_TIME, self(), circulation_persist)
               end,
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
persist() ->
    Size = ets:info(?TAB, size),
    if
        Size =/= undefined andalso Size =/= 0 ->
            logger:info("db_persister, Size: ~p~n", [Size]),
            FirstKey = ets:first(?TAB),
            {_, FailSet} = misc_utils:upto(fun(_Idx, {Key, Except}) ->
                if
                    Key =:= '$end_of_table' -> 
                        {break, ok};
                    true -> 
                        do_persist(Key, Except)
                end
            end, {FirstKey, sets:new()}, 1, Size),
            case sets:size(FailSet) of
                0 -> persist();
                _ -> ok
            end;
        true -> 
            ok
    end,
    erlang:send_after(?SLEEP_TIME, self(), circulation_persist).

do_persist(Key, Except) ->
    % logger:info("do_persist, Key: ~p, Except: ~p~n", [Key, sets:to_list(Except)]),
    case ets:lookup(?TAB, Key) of
        [] -> {ets:next(?TAB, Key), Except};
        [{Key, PlayerID, Version, Cmds}] ->
            % logger:info("do_persist, Key: ~p, PlayerID: ~p, Version: ~p, Cmds: ~p~n",
            %             [Key, PlayerID, Version, Cmds]),
            case sets:is_element(PlayerID, Except) of
                true -> {ets:next(?TAB, Key), Except};
                false ->
                    try execute_persist(PlayerID, Cmds, Version) of
                        _ -> 
                            ets:update_counter(?TAB_COUNTER, PlayerID, -1),
                            NextKey = ets:next(?TAB, Key),
                            ets:delete(?TAB, Key),
                            {NextKey, Except}
                    catch
                        Type:Msg ->
                            Args = [{player_id, PlayerID}, {key, Key}],
                            exception:notify(Type, Args, Msg),
                            {ets:next(?TAB, Key), sets:add_element(PlayerID, Except)}
                    end
            end
    end.

execute_persist(PlayerID, Cmds, Version) ->
    case configuration:get(gs_config, data_driver) of
        mysql ->
            %% MySQL
            Sql = binary_string:join(Cmds, <<";">>),
            execute_with_procedure(PlayerID, Sql, Version);
        _ ->
            %% Redis
            redis:transaction(Cmds)
    end.

execute_with_procedure(PlayerID, Sql, Version) ->
    ProcedureName = db:procedure_name(<<"player">>, PlayerID),
    % logger:info("PERSIST FOR [~p](~p) Sql: ~p~n", [PlayerID, Version, Sql]),
    case db:find_by(schema_persistances, uuid, PlayerID) of
        {ok, []} ->
            Insert = db_fmt:format("INSERT INTO `schema_persistances` (`uuid`, `version`) VALUES (~s, ~s)", 
                                   [db_fmt:encode(PlayerID), db_fmt:encode(Version)]),
            SQL = list_to_binary([Sql, <<";">>, Insert]),
            db:execute_with_procedure(ProcedureName, SQL);
        {ok, [Schema]} ->
            OldVersion = record_mapper:get_field(version, Schema),
            if 
                OldVersion =:= Version -> ok;
                OldVersion < Version ->
                    Update = db_fmt:format("UPDATE `schema_persistances` SET `version` = ~s WHERE `schema_persistances`.`uuid` = ~s", 
                                           [db_fmt:encode(Version), db_fmt:encode(PlayerID)]),
                    SQL = list_to_binary([Sql, <<";">>, Update]),
                    db:execute_with_procedure(ProcedureName, SQL)
            end
    end.
