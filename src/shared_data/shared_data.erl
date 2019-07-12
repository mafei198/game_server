-module(shared_data).
-behaviour(gen_server).

-export([new/2]).

-export([find/3,
         find/2,
         write/1,
         update/3,
         update_counter/3,
         delete/2,
         select/2,
         each/2,
         migrate/2]).

-define(SHARED_DATA_SERVER, <<"__SHARED_DATA_SERVER__">>).
-define(TRACKER_TAB, shared_data_tracker).

-include("../app/include/game_const.hrl").
-include("../app/include/custom_record.hrl").

%% API
-export([start_link/0,
         sync_persist/0]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PERSIST_DURATION, 1000). %% 6 minutes, 单位(毫秒)
-define(BATCH_AMOUNT, 50).

-record(state, {circulation_persist_timer,
                tabs = sets:new()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new(Name, Options) ->
    gen_server:call(?SERVER, {new, Name, Options}, infinity).

sync_persist() ->
    gen_server:call(?SERVER, {sync_persist}).

find(Table, Key, Default) ->
    case ets:lookup(Table, Key) of
        [] -> Default;
        [Rec] -> Rec
    end.

find(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] -> undefined;
        [Rec] -> Rec
    end.

write(Record) ->
    true = ets:insert(element(1, Record), Record),
    track_write(Record),
    Record.

update(Table, Key, KVS) ->
    case ets:lookup(Table, Key) of
        [] -> undefined;
        [Rec] ->
            NewRec = record_mapper:set_fields(KVS, Rec),
            write(NewRec)
    end.

update_counter(Table, Key, Amount) ->
    Result = ets:update_counter(Table, Key, Amount, {Table, Key, 0}),
    track_counter(Table, Key, Amount),
    Result.

delete(Table, Key) ->
    true = ets:delete(Table, Key),
    track_delete(Table, Key),
    ok.

select(Table, MatchSpec) ->
    ets:select(Table, MatchSpec).

migrate(Table, TransformFun) ->
    each(Table, fun(OldRec) ->
        NewRec = TransformFun(OldRec),
        if
            NewRec =:= OldRec -> {break, already_migrated_break};
            true -> write(NewRec)
        end
    end),
    migrate_success.

each(Table, Fun) ->
    each(Table, ets:first(Table), Fun).

each(_Table, '$end_of_table', _Fun) -> ok;
each(Table, Key, Fun) ->
    Value = find(Table, Key),
    case Fun(Value) of
        {break, V} -> V;
        true -> each(Table, ets:next(Key), Fun)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    file:make_dir("dets_files"),
    data_holder:add_ets(shared_data_tracker, [named_table, public, ordered_set, {keypos, 1},
                                              {read_concurrency, true}]),
    process_flag(trap_exit, true),
    Timer = erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data),
    {ok, #state{circulation_persist_timer = Timer}}.

handle_call({new, Name, Options}, _From, State) ->
    Name = data_holder:add_ets(Name, Options),
    add_dets(Name, Options),
    {reply, ok, State#state{tabs = sets:add_element(Name, State#state.tabs)}};
handle_call({sync_persist}, _From, State=#state{circulation_persist_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    NewTimer = batch_persist(),
    {reply, true, State#state{circulation_persist_timer = NewTimer}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({track_counter, RecName, RecId, _Amount}, State) ->
    case ets:lookup(RecName, RecId) of
        [] -> ok;
        [Rec] -> track_write(Rec)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(circulation_persist_data, State=#state{circulation_persist_timer=Timer}) ->
    erlang:cancel_timer(Timer),
    NewTimer = batch_persist(),
    {noreply, State#state{circulation_persist_timer=NewTimer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    lists:foreach(fun(Table) ->
        ok = dets:close(Table)
    end, sets:to_list(State#state.tabs)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

track_write(Rec) ->
    Counter = game_counter:incr_tmp({?MODULE, tracker}),
    % logger:info("track_write, Counter: ~p, Rec: ~p~n", [Counter, Rec]),
    ets:insert(?TRACKER_TAB, {Counter, {create, Rec}}).

track_delete(RecName, RecId) ->
    ets:insert(?TRACKER_TAB, {game_counter:incr_tmp({?MODULE, tracker}), 
                              {delete, RecName, RecId}}).

track_counter(RecName, RecId, Amount) ->
    gen_server:cast(?SERVER, {track_counter, RecName, RecId, Amount}).

batch_persist() ->
    Size = ets:info(?TRACKER_TAB, size),
    % ets:foldl(fun(Rec, Acc) ->
    %     logger:info("rec in queue: ~p~n", [Rec])
    % end, [], ?TRACKER_TAB),
    % logger:info("shared_data batch_persist: ~p~n", [Size]),
    if
        Size =:= 0 orelse Size =:= undefined ->
            erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data);
        true ->
            batch_create(lists:min([Size, ?BATCH_AMOUNT]))
    end.

%% {Counter, {create, Rec}|{delete, RecName, RecId}}
batch_create(N) ->
    misc_utils:upto(fun(_) ->
        Key = ets:first(?TRACKER_TAB),
        [{_Counter, Log}] = ets:lookup(?TRACKER_TAB, Key),
        case Log of
            {create, Rec} ->
                % logger:info("batch_persist, ~p, insert: ~p~n", 
                %             [TimeStamp, Rec]),
                ok = dets:insert(element(1, Rec), Rec);
            {delete, RecName, RecId} ->
                % logger:info("batch_persist, ~p, delete: ~p, RecId: ~p~n", 
                %             [TimeStamp, RecName, RecId]),
                ok = dets:delete(RecName, RecId)
        end,
        ets:delete(?TRACKER_TAB, Key)
    end, 1, N),
    batch_persist().

read_write_test() ->
    % misc_utils:upto(fun(N) ->
    %     spawn(fun() ->
    %         misc_utils:upto(fun(Idx) ->
    %             timer:sleep(rand:uniform(1000)),
    %             shared_data:write(#ws_user_shareds{uuid = N * 10000 + Idx})
    %         end, 1, 10000)
    %     end)
    % end, 1, 3000),
    misc_utils:upto(fun(_x) ->
        spawn(fun() ->
            misc_utils:upto(fun(_Idx) ->
                timer:sleep(rand:uniform(1000)),
                shared_data:find(ws_user_shareds, 1)
            end, 1, 10000)
        end)
    end, 1, 3000).

add_dets(Table, Options) ->
    {ok, Path} = file:get_cwd(),
    case dets:info(Table) of
        undefined ->
            dets:open_file(Table, [{keypos, proplists:get_value(keypos, Options, 2)}, 
                                   {type, detect_type(Options)},
                                   {file, Path ++ "/dets_files/" ++ atom_to_list(Table)}]),
            case dets:info(Table, size) of
                0 -> ok;
                _ -> ets:from_dets(Table, Table)
            end;
        _Info ->
            error_logger:warning_msg("shared_data, Dets Table: [~p], already opened~n", 
                                     [Table]),
            alread_exists
    end.

-define(SHARED_DATA_TYPES, [bag, duplicate_bag, set]).
detect_type(Options) ->
    case lists:member(ordered_set, Options) of
        true -> erlang:error(unsupported_shared_data_type, ordered_set);
        false ->
            misc_utils:each(fun(Type) ->
                case lists:member(Type, Options) of
                    true -> {break, Type};
                    false -> set
                end
            end, ?SHARED_DATA_TYPES)
    end.
