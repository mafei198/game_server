-module(redis).

-export([create/2, gen_create_cmd/3,
         update/3, gen_update_cmd/4,
         delete/1, gen_delete_cmd/2,
         delete/3, gen_delete_cmd/4,
         find/2, 
         select/2,
         count/1,
         all/1,
         all_keys/1,
         delete_all/1,
         transaction/1,
         add_search/3,
         del_search/2,
         search/2]).

create(Record, ForeignKeys) ->
    Cmds = gen_create_cmd(Record, ForeignKeys, []),
    case ForeignKeys of
        [] -> {ok, _} = eredis_pool:q(redis, lists:nth(1, Cmds));
        _ -> transaction(Cmds)
    end.

gen_create_cmd(Record, ForeignKeys, Result) ->
    {RecName, KVList} = record_to_kvlist(Record),
    Uuid = record_mapper:get_field(uuid, Record),
    Key = gen_key(RecName, Uuid),
    CollectionKey = gen_collection_key(RecName),
    ResultN = [["SADD", CollectionKey, Key], ["HMSET", Key|KVList]|Result],
    lists:foldl(fun(K, Acc) ->
        V = record_mapper:get_field(K, Record),
        IndexKey = gen_index_key(RecName, K, V),
        [["SADD", IndexKey, Key]|Acc]
    end, ResultN, ForeignKeys).

update(RecName, Uuid, KVList) ->
    eredis_pool:q(redis, gen_update_cmd(RecName, Uuid, KVList, [])).

gen_update_cmd(RecName, Uuid, KVList, Result) ->
    Key = gen_key(RecName, Uuid),
    [["HMSET", Key|KVList]|Result].

delete(Rec) ->
    RecName = element(1, Rec),
    Uuid = record_mapper:get_field(uuid, Rec),
    ForeignKVList = model:gen_foreign_kvlist(Rec),
    delete(RecName, Uuid, ForeignKVList).

delete(RecName, Uuid, ForeignKVList) ->
    Cmds = gen_delete_cmd(RecName, Uuid, ForeignKVList, []),
    case ForeignKVList of
        [] -> eredis_pool:q(redis, lists:nth(1, Cmds));
        _ -> transaction(Cmds)
    end.

gen_delete_cmd(Rec, Result) ->
    RecName = element(1, Rec),
    Uuid = record_mapper:get_field(uuid, Rec),
    ForeignKVList = model:gen_foreign_kvlist(Rec),
    gen_delete_cmd(RecName, Uuid, ForeignKVList, Result).

gen_delete_cmd(RecName, Uuid, ForeignKVList, Result) ->
    Key = gen_key(RecName, Uuid),
    CollectionKey = gen_collection_key(RecName),
    ResultN = [["SREM", CollectionKey, Key], ["DEL", Key]|Result],
    lists:foldl(fun({K, V}, Acc) ->
        IndexKey = gen_index_key(RecName, K, V),
        [["SREM", IndexKey, Key]|Acc]
    end, ResultN, ForeignKVList).

find(RecName, Uuid) when Uuid =/= undefined ->
    Key = gen_key(RecName, Uuid),
    case eredis_pool:q(redis, ["HGETALL", Key]) of
        {ok, []} -> {ok, []};
        {ok, KVList} -> 
            {ok, [record_from_kvlist(RecName, KVList)]}
    end.

select(RecName, {IndexName, IndexValue}) ->
    IndexKey = gen_index_key(RecName, IndexName, IndexValue),
    case eredis_pool:q(redis, ["SMEMBERS", IndexKey]) of
        {ok, []} -> {ok, []};
        {ok, Keys} -> batch_get(RecName, Keys)
    end.

count(RecName) ->
    CollectionKey = gen_collection_key(RecName),
    {ok, Count} = eredis_pool:q(redis, ["SCARD", CollectionKey]),
    binary_to_integer(Count).

all(RecName) ->
    Keys = all_keys(RecName),
    batch_get(RecName, Keys).

all_keys(RecName) ->
    CollectionKey = gen_collection_key(RecName),
    case eredis_pool:q(redis, ["SMEMBERS", CollectionKey]) of
        {ok, []} -> [];
        {ok, Keys} -> Keys
    end.

delete_all(RecName) ->
    Cmds = [["DEL", Key] || Key <- all_keys(RecName)],
    eredis_pool:qp(redis, Cmds).
    % {ok, Recs} = all(Redis, RecName),
    % Cmds = lists:foldl(fun(Rec, Acc) ->
    %     gen_delete_cmd(Rec, Acc)
    % end, [], Recs),
    % eredis:qp(Redis, Cmds).

transaction(Cmds) ->
    {ok, _} = eredis_pool:transaction(redis, fun(Redis) ->
        eredis:qp(Redis, Cmds)
    end).

add_search(Topic, Key, Value) ->
    {ok, _} = eredis_pool:q(redis, ["HSET", gen_search_topic(Topic), Key, Value]).

del_search(Topic, Key) ->
    {ok, _} = eredis_pool:q(redis, ["HDEL", gen_search_topic(Topic), Key]).

search(Topic, Pattern) ->
    do_search(Topic, 0, Pattern, []).

do_search(_Topic, <<"0">>, _Pattern, Result) -> Result;
do_search(Topic, Cursor, Pattern, Result) ->
    {ok, [NCursor, List]} = eredis_pool:q(redis, ["HSCAN", gen_search_topic(Topic), Cursor, "MATCH", Pattern]),
    NResult = list_to_pairlist(List, Result),
    do_search(Topic, NCursor, Pattern, NResult).

gen_search_topic(Topic) -> 
    "SearchKVS:"++Topic.

batch_get(_RecName, []) -> {ok, []};
batch_get(RecName, Keys) ->
    % logger:info("batch_get Keys: ~p~n", [Keys]),
    Cmds = gen_pipe_hget(Keys),
    Results = eredis_pool:qp(redis, Cmds),
    % logger:info("batch_get Results: ~p, Cmds: ~p~n", [Results, Cmds]),
    Recs = lists:foldl(fun({ok, KVList}, Acc) ->
        case KVList of
            [] -> Acc;
            _ -> [record_from_kvlist(RecName, KVList)|Acc]
        end
    end, [], Results),
    {ok, Recs}.

record_from_kvlist(_RecName, []) -> undefined;
record_from_kvlist(RecName, KVList) ->
    FieldTypes = schema_type:field_types(RecName),
    KVPairs = kvlist_to_proplist(KVList, FieldTypes, []),
    Fields = record_mapper:info(RecName, fields),
    Values = record_values(Fields, KVPairs, []),
    list_to_tuple([RecName|Values]).

kvlist_to_proplist([], _FieldTypes, Result) -> Result;
kvlist_to_proplist([K, V|KVList], FieldTypes, Result) ->
    Field = binary_to_atom(K, utf8),
    Type = proplists:get_value(Field, FieldTypes),
    Value = if
                Type =:= integer orelse Type =:= boolean ->
                    case V of
                        undefined -> 0;
                        V -> 
                            {I, _} = string:to_integer(binary_to_list(V)),
                            I
                    end;
                Type =:= float ->
                    case V of
                        undefined -> 0;
                        V ->
                            case string:to_float(binary_to_list(V)) of
                                {error, no_float} -> binary_to_integer(V);
                                {F, _} -> F
                            end
                    end;
                Type =:= text orelse Type =:= string ->
                    V;
                true ->
                    error_logger:warning_msg("~p unparsed value type: ~p, k:~p, v: ~p~n", 
                                             [?MODULE, Type, K, V]),
                    V
            end,
    kvlist_to_proplist(KVList, FieldTypes, [{Field, Value}|Result]).

record_values([], _KVPairs, Result) -> lists:reverse(Result);
record_values([Field|Fields], KVPairs, Result) ->
    Value = proplists:get_value(Field, KVPairs),
    record_values(Fields, KVPairs, [Value|Result]).

gen_pipe_hget(Keys) ->
    lists:foldl(fun(Key, Acc) ->
        [["HGETALL", Key]|Acc]
    end, [], Keys).

gen_key(Name, Uuid) ->
    % logger:info("gen_key Name: ~p, Uuid: ~p~n", [Name, Uuid]),
    BName = atom_to_binary(Name, utf8),
    <<BName/binary, <<":">>/binary, Uuid/binary>>.

gen_collection_key(Name) ->
    BName = atom_to_binary(Name, utf8),
    <<BName/binary, <<":_collections_">>/binary>>.

gen_index_key(RecName0, IndexName0, IndexValue) ->
    RecName = atom_to_binary(RecName0, utf8),
    IndexName = atom_to_binary(IndexName0, utf8),
    <<RecName/binary, <<":">>/binary, IndexName/binary, <<":">>/binary, IndexValue/binary>>.

%% Private
record_to_kvlist(Record) ->
    [Name|Values] = tuple_to_list(Record),
    Fields = record_mapper:info(Name, fields),
    {Name, combine(Fields, Values, [])}.

combine([], [], Result) -> Result;
combine([_|Fields], [undefined|Values], Result) ->
    combine(Fields, Values, Result);
combine([Field|Fields], [Value|Values], Result) ->
    case is_float(Value) of
        true ->
            combine(Fields, Values, [Field, float_to_list(Value)|Result]);
        false ->
            combine(Fields, Values, [Field, Value|Result])
    end.

list_to_pairlist([], Result) -> Result;
list_to_pairlist([_, undefined|List], Result) ->
    list_to_pairlist(List, Result);
list_to_pairlist([K, V|List], Result) ->
    list_to_pairlist(List, [{K, V}|Result]).
