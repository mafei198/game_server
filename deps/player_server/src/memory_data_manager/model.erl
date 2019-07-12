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

-module(model).
-export([update/2,
         update/1,
         find/1,
         find/2,
         find_or_create/1,
         all/1,
         all/2,
         id_status_list/1,
         count_all/1,
         where/1,
         delete/1,
         create/1,
         create/2,
         count/1,
         get_persist_all_cmds/0,
         persist_all/0,
         ensure_load_data/1,
         get_foreign_keys_info/2,
         gen_foreign_kvlist/1]).

-define(MODEL_ORIGIN, 1).
-define(MODEL_UPDATE, 2).
-define(MODEL_DELETE, 3).
-define(MODEL_CREATE, 4).

-define(QUEUE_PERSIST_KEY, {tmp, queue_persist_key}).
-define(NO_CACHE_TABLES_KEY, {tmp, no_cache_tables}).

find(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case record_mapper:get_field(uuid, Selector) of
        undefined -> 
            check_is_empty_record(Values),
            selectOne(Table, Values);
        Id -> get({Table, Id})
    end.

check_is_empty_record(Values) ->
    Set = sets:from_list(Values),
    NSet = sets:del_element(undefined, Set),
    case sets:size(NSet) of
        0 -> error(empty_record);
        _ -> false
    end.

find(Table, Id) when Id =/= undefined ->
    ensure_load_data(Table),
    get({Table, Id}).

find_or_create(Selector) ->
    case find(Selector) of
        undefined -> create(Selector);
        Rec -> Rec
    end.

count_all(Table) ->
    ensure_load_data(Table),
    length(id_status_list(Table)).

all(Table) ->
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            lists:foldl(fun
                ({Id, _}, Result) ->
                    case get({Table, Id}) of
                        undefined -> Result;
                        Rec -> [Rec|Result]
                    end
            end, [], IdList)
    end.

all(Table, {Offset, Limit}) ->
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            SubIdList = lists:sublist(IdList, Offset, Limit),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    case get({Table, Id}) of
                        undefined -> Result;
                        Rec -> [Rec|Result]
                    end
            end, [], SubIdList)
    end.

where(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            Fields = record_mapper:get_mapping(Table),
            FieldsAndValues = makepat(Fields, Values),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  -> [Rec|Result];
                        false -> Result
                    end
            end, [], IdList)
    end.

count(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    Fields = record_mapper:get_mapping(Table),
    case makepat(Fields, Values) of
        [] -> length(id_status_list(Table));
        FieldsAndValues ->
            case id_status_list(Table) of
                [] -> 0;
                IdList ->
                    lists:foldl(fun
                        ({Id, _}, Result) ->
                            Rec = get({Table, Id}),
                            case match(Rec, FieldsAndValues) of
                                true  -> Result + 1;
                                false -> Result
                            end
                    end, 0, IdList)
            end
    end.

update(Record) ->
    [Table|Values] = tuple_to_list(Record),
    check_is_empty_record(Values),
    case record_mapper:get_field(uuid, Record) of
        undefined -> error(invalid_update, Record);
        Id ->
            case get({Table, Id}) of
                undefined -> undefined;
                _ ->
                    update_status(Table, Id, ?MODEL_UPDATE),
                    put({Table, Id}, Record),
                    invoke_after_update(Record),
                    Record
            end
    end.

update(Selector, Modifier) ->
    [Table|_] = tuple_to_list(Selector),
    check_is_empty_record(tl(tuple_to_list(Modifier))),
    ensure_load_data(Table),
    case record_mapper:get_field(uuid, Selector) of
        undefined -> match_update(Table, Selector, Modifier);
        Id -> update_by_key(Table, Id, Modifier)
    end.

update_by_key(Table, Id, Modifier) ->
    case get({Table, Id}) of
        undefined -> ok;
        Rec ->
            check_is_empty_record(tl(tuple_to_list(Modifier))),
            update_status(Table, Id, ?MODEL_UPDATE),
            NewRec = update_record(Rec, Modifier),
            put({Table, Id}, NewRec),
            invoke_after_update(NewRec),
            NewRec
    end.

match_update(Table, Selector, Modifier) ->
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            Fields = record_mapper:get_mapping(Table),
            [_|Values] = tuple_to_list(Selector),
            FieldsAndValues = makepat(Fields, Values),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  ->
                            update_status(Table, Id, ?MODEL_UPDATE),
                            NewRec = update_record(Rec, Modifier),
                            put({Table, Id}, NewRec),
                            invoke_after_update(NewRec),
                            [NewRec|Result];
                        false -> 
                            Result
                    end
            end, [], IdList)
    end.

delete(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case record_mapper:get_field(uuid, Selector) of
        undefined ->
            Fields = record_mapper:get_mapping(Table),
            FieldsAndValues = makepat(Fields, Values),
            match_delete(Table, FieldsAndValues);
        Id ->
            update_status(Table, Id, ?MODEL_DELETE),
            set_foreign_keys_info(Table, Id),
            Rec = erase({Table, Id}),
            invoke_after_delete(Rec),
            Rec
    end.

get_foreign_keys_info(Table, Id) ->
    get({deleted, Table, Id, foreign_kvlist}).

set_foreign_keys_info(Table, Id) ->
    put({deleted, Table, Id, foreign_kvlist}, gen_foreign_kvlist(Table, Id)).

del_foreign_keys_info(Table, Id) ->
    erase({deleted, Table, Id, foreign_kvlist}).

gen_foreign_kvlist(Table, Id) ->
    ForeignKeys = get_foreign_keys(Table),
    case find(Table, Id) of
        undefined -> [];
        Rec ->
            [{K, record_mapper:get_field(K, Rec)} || K <- ForeignKeys]
    end.

gen_foreign_kvlist(undefined) -> [];
gen_foreign_kvlist(Rec) ->
    Table = element(1, Rec),
    ForeignKeys = get_foreign_keys(Table),
    [{K, record_mapper:get_field(K, Rec)} || K <- ForeignKeys].

get_foreign_keys(Table) ->
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    code:ensure_loaded(Module),
    case proplists:get_value(foreign_keys, Module:module_info(attributes)) of
        undefined -> [];
        ForeignKeys -> ForeignKeys
    end.

match_delete(Table, FieldsAndValues) ->
    case id_status_list(Table) of
        [] -> ok;
        IdList ->
            lists:foreach(fun
                ({Id, _}) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  ->
                            update_status(Table, Id, ?MODEL_DELETE),
                            set_foreign_keys_info(Table, Id),
                            invoke_after_delete(Rec),
                            erase({Table, Id});
                        false -> undefined
                    end
            end, IdList),
            ok
    end.

%% create new record.
create(Records) when is_list(Records) ->
    lists:foreach(fun create/1, Records);
create(Record) ->
    {RecWithId, Id} = case record_mapper:get_field(uuid, Record) of
        undefined ->
            NewId = uuid_factory:gen(),
            {record_mapper:set_field(uuid, NewId, Record), NewId};
        Uuid -> {Record, Uuid}
    end,
    [Table|_Values] = tuple_to_list(RecWithId),
    ensure_load_data(Table),
    update_status(Table, Id, ?MODEL_CREATE),
    put({Table, Id}, RecWithId),
    invoke_after_create(RecWithId),
    RecWithId.

%% Load data from databse.
create(Record, load) ->
    [Table|_] = tuple_to_list(Record),
    Id = record_mapper:get_field(uuid, Record),
    update_status(Table, Id, ?MODEL_ORIGIN),
    put({Table, Id}, Record).

generate_persist_cmds(Table, Result) ->
    StatusIdList = id_status_list(Table),
    DeleteIdList = delete_status_list(Table),
    if
        StatusIdList =:= [] andalso DeleteIdList =:= [] -> Result;
        true -> persist_cmds(Table, StatusIdList ++ DeleteIdList, Result)
    end.

persist_all() ->
    try do_persist_all() of
        Result -> Result
    catch
        Type:Msg ->
            exception:notify(Type, [<<"PlayerID">>, get(player_id)], Msg)
    end.

do_persist_all() ->
    case get_persist_all_cmds() of
        [] -> ok;
        Cmds -> 
            persist_queue_add(Cmds),
            Tables = all_loaded_tables(),
            reset_tables_status(Tables)
    end.

get_persist_all_cmds() ->
    Tables = all_loaded_tables(),
    % PlayerID = get(player_id),
    % logger:info("PERSIST FOR: ~p~n", [PlayerID]),
    lists:foldl(fun(Table, Result) ->
        generate_persist_cmds(Table, Result)
    end, [], Tables).

reset_tables_status(Tables) ->
    lists:foreach(fun reset_status/1, Tables).

reset_status(Table) ->
    lists:foreach(fun({Id, _}) ->
        del_foreign_keys_info(Table, Id)
    end, delete_status_list(Table)),
    put({Table, deleteIdList}, []),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            case is_no_cache_table(Table) of
                true ->
                    lists:foreach(fun({Id, _}) ->
                        erase({Table, Id})
                    end, IdList),
                    erase({Table, idList});
                false ->
                    NewIdList = lists:foldl(fun({Id, _}, Result) ->
                                    [{Id, ?MODEL_ORIGIN}|Result]
                                end, [], IdList),
                    put({Table, idList}, lists:reverse(NewIdList))
            end
    end.

persist_queue_add(Cmds) ->
    Version = game_counter:incr({schema_persistances, version, get(player_id)}),
    db_persister:add(get(player_id), Version, Cmds).

%% Private Methods
selectOne(Table, Values) ->
    Fields = record_mapper:get_mapping(Table),
    case id_status_list(Table) of
        [] -> undefined;
        IdList ->
            FieldsAndValues = makepat(Fields, Values),
            selectOne(Table, IdList, FieldsAndValues)
    end.

selectOne(_, [], _) ->
    undefined;
selectOne(Table, [{Id, _}|IdList], FieldsAndValues) ->
    Rec = get({Table, Id}),
    case match(Rec, FieldsAndValues) of
        true -> Rec;
        false -> selectOne(Table, IdList, FieldsAndValues)
    end.

update_status(Table, Id, Status) ->
    IdList = id_status_list(Table),
    case lists:keyfind(Id, 1, IdList) of
        false ->
            if 
                Status =:= ?MODEL_DELETE -> ok;
                Status =:= ?MODEL_ORIGIN orelse Status =:= ?MODEL_CREATE ->
                    put({Table, idList}, [{Id, Status}|IdList])
            end;
        {Id, ?MODEL_CREATE} ->
            if 
                Status =:= ?MODEL_UPDATE -> ok;
                Status =:= ?MODEL_DELETE ->
                    NewIdList = lists:delete({Id, ?MODEL_CREATE}, IdList),
                    put({Table, idList}, NewIdList)
            end;
        {Id, ?MODEL_UPDATE} ->
            if 
                Status =:= ?MODEL_UPDATE -> ok;
                Status =:= ?MODEL_DELETE ->
                    NewIdList = lists:delete({Id, ?MODEL_UPDATE}, IdList),
                    put({Table, idList}, NewIdList),
                    add_to_delete_list(Table, Id)
            end;
        {Id, ?MODEL_ORIGIN} ->
            if 
                Status =:= ?MODEL_UPDATE ->
                    NewIdList = lists:keyreplace(Id, 1, IdList, {Id, Status}),
                    put({Table, idList}, NewIdList);
                Status =:= ?MODEL_DELETE ->
                    NewIdList = lists:delete({Id, ?MODEL_ORIGIN}, IdList),
                    put({Table, idList}, NewIdList),
                    add_to_delete_list(Table, Id)
            end
    end.

makepat(Fields, Values) ->
    makepat(Fields, Values, []).

makepat([], [], Result) ->
    lists:reverse(Result);
makepat([_Field|Fields], [Value|Values], Result) when Value =:= undefined ->
    makepat(Fields, Values, Result);
makepat([Field|Fields], [Value|Values], Result) ->
    makepat(Fields, Values, [{Field, Value}|Result]).

match(_Record, []) -> true;
match(Record, [{Field, Value}|FieldsAndValues]) ->
    case record_mapper:get_field(Field, Record) of
        Value -> match(Record, FieldsAndValues);
        _ -> false
    end.

ensure_load_data(Table) ->
    case is_table_loaded(Table) of
        true  -> true;
        false ->
            PlayerID = get(player_id),
            Module = list_to_atom(atom_to_list(Table) ++ "_model"),
            case Module:load_data(PlayerID) of
                {ok, no_cache} -> record_no_cache_table(Table);
                {ok, []} -> ok;
                {ok, Recs} -> insert_recs(Recs, Module)
            end,
            record_loaded_table(Table),
            case erlang:function_exported(Module, after_load_data, 1) of
                true -> Module:after_load_data(PlayerID);
                false -> ok
            end
    end.

record_no_cache_table(Table) ->
    case get(?NO_CACHE_TABLES_KEY) of
        undefined -> put(?NO_CACHE_TABLES_KEY, [Table]);
        List -> put(?NO_CACHE_TABLES_KEY, [Table|List])
    end.

is_no_cache_table(Table) ->
    case get(?NO_CACHE_TABLES_KEY) of
        undefined -> false;
        List -> lists:member(Table, List)
    end.

is_table_loaded(Table) ->
    case get({loaded, Table}) of
        true -> true;
        _ -> false
    end.

record_loaded_table(Table) ->
    put({loaded, Table}, true),
    case get({loaded, tables}) of
        undefined ->
            put({loaded, tables}, [Table]);
        Tables ->
            case lists:member(Table, Tables) of
                true -> ok;
                false -> put({loaded, tables}, [Table|Tables])
            end
    end.

all_loaded_tables() ->
    case get({loaded, tables}) of
        undefined -> [];
        Tables when is_list(Tables) -> Tables
    end.

id_status_list(Table) ->
    case get({Table, idList}) of
        undefined -> [];
        IdList -> IdList
    end.

delete_status_list(Table) ->
    case get({Table, deleteIdList}) of
        undefined -> [];
        IdList -> IdList
    end.

persist_cmds(Table, IdList, Result) ->
    case configuration:get(gs_config, data_driver) of
        mysql ->
            % MySQL
            lists:foldl(fun({Id, Status}, Acc) ->
                sql_generator:sql(Table, Id, Status, fun get/1, Acc)
            end, Result, IdList);
        _ ->
            % Redis
            lists:foldl(fun({Id, Status}, Acc) ->
                redis_generator:cmd(Table, Id, Status, fun get/1, Acc)
            end, Result, IdList)
    end.

add_to_delete_list(Table, Id) ->
    List = get({Table, deleteIdList}),
    if
        List =:= undefined orelse List =:= [] ->
            put({Table, deleteIdList}, [{Id, ?MODEL_DELETE}]);
        true ->
            put({Table, deleteIdList}, [{Id, ?MODEL_DELETE}|List])
    end.

update_record(Record, Modifier) ->
    [Table|RecordValues] = tuple_to_list(Record),
    [_|ModifierValues] = tuple_to_list(Modifier),
    Values = update_record(RecordValues, ModifierValues, []),
    list_to_tuple([Table|Values]).

update_record([], [], Result) ->
    lists:reverse(Result);
update_record([RecordValue|RecordValues], [ModifierValue|ModifierValues], Result) when ModifierValue =:= undefined ->
    update_record(RecordValues, ModifierValues, [RecordValue|Result]);
update_record([_RecordValue|RecordValues], [ModifierValue|ModifierValues], Result)->
    update_record(RecordValues, ModifierValues, [ModifierValue|Result]).

insert_recs(Recs, Module) ->
    case lists:keyfind(serialize, 1, Module:module_info(attributes)) of
        false ->
            lists:foreach(fun(Rec) -> create(Rec, load) end, Recs);
        {serialize, Rule} ->
            Fields = record_mapper:get_mapping(hd(tuple_to_list(hd(Recs)))),
            lists:foreach(fun(Rec) -> 
                NewRec = deserialize(Rec, Fields, Rule),
                create(NewRec, load) 
            end, Recs)
    end.

deserialize(Rec, Fields, Rule) ->
    [RecName|Values] = tuple_to_list(Rec),
    TermValues = deserialize(Values, Fields, Rule, []),
    list_to_tuple([RecName|TermValues]).

deserialize([], [], _Rule, Result) -> 
    lists:reverse(Result);
deserialize([Value|Values], [Field|Fields], Rule, Result) ->
    case lists:member(Field, Rule) of
        true when Value =/= undefined -> 
            TermValue = case base64:decode(Value) of
                <<>> -> undefined;
                Data -> binary_to_term(Data)
            end,
            deserialize(Values, Fields, Rule, [TermValue|Result]);
        _ ->
            deserialize(Values, Fields, Rule, [Value|Result])
    end.

invoke_after_update(Record) ->
    Table = element(1, Record),
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    case erlang:function_exported(Module, after_update_callback, 1) of
        true -> Module:after_update_callback(Record);
        false -> ok
    end.

invoke_after_delete(undefined) -> ok;
invoke_after_delete(Record) ->
    Table = element(1, Record),
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    case erlang:function_exported(Module, after_delete_callback, 1) of
        true -> Module:after_delete_callback(Record);
        false -> ok
    end.

invoke_after_create(Record) ->
    Table = element(1, Record),
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    case erlang:function_exported(Module, after_create_callback, 1) of
        true -> Module:after_create_callback(Record);
        false -> ok
    end.
