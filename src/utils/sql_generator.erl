-module(sql_generator).

-export([sql/5, 
         gen_create_sql/1,
         gen_batch_create_sql/1]).

-define(MODEL_ORIGIN, 1).
-define(MODEL_UPDATE, 2).
-define(MODEL_DELETE, 3).
-define(MODEL_CREATE, 4).

sql(Table, Id, Status, Finder, Result) ->
    if
        Status =:= ?MODEL_ORIGIN -> Result;
        Status =:= ?MODEL_CREATE orelse Status =:= ?MODEL_UPDATE ->
            Rec = Finder({Table, Id}),
            [sql(Rec, Status)|Result];
        Status =:= ?MODEL_DELETE ->
            [sql({Table, Id}, Status)|Result]
    end.

sql(Rec, ?MODEL_CREATE) ->
    {Table, Fields, Values} = rec_info(Rec),
    db_fmt:format("INSERT INTO `~s` (~s) VALUES (~s)", 
                  [Table, join_fields(Fields), join_values(Values)]);
sql(Rec, ?MODEL_UPDATE) ->
    {Table, Fields, Values} = rec_info(Rec),
    Uuid = record_mapper:get_field(uuid, Rec),
    db_fmt:format("UPDATE `~s` SET ~s WHERE `uuid` = ~s", 
                  [Table, db_fmt:map(Fields, Values), db_fmt:encode(Uuid)]);
sql({Table, Uuid}, ?MODEL_DELETE) ->
    db_fmt:format("DELETE FROM `~s` WHERE `uuid` = ~s", 
                  [Table, db_fmt:encode(Uuid)]).

gen_create_sql(Rec) ->
    sql(Rec, ?MODEL_CREATE).

gen_batch_create_sql(Recs) ->
    {Table, Fields, _} = rec_info(hd(Recs)),
    BatchValues = lists:foldl(fun(Rec, Acc) ->
        Values = tl(tuple_to_list(Rec)),
        [binary_string:join([<<"(">>, join_values(Values), <<")">>], <<"">>)|Acc]
    end, [], Recs),
    db_fmt:format("INSERT INTO `~s` (~s) VALUES ~s;",
                  [Table, join_fields(Fields), binary_string:join(BatchValues, <<",">>)]).

rec_info(Rec) ->
    [Table|Values] = tuple_to_list(Rec),
    Fields = record_mapper:get_mapping(Table),
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    case erlang:module_loaded(Module) of
        true ->
            Rule = proplists:get_value(serialize, Module:module_info(attributes)),
            SerializedValues = serialize(Values, Fields, Rule),
            {Table, Fields, SerializedValues};
        false ->
            {Table, Fields, Values}
    end.

serialize(Values, _Fields, undefined) -> Values;
serialize(Values, Fields, Rule) ->
    serialize(Values, Fields, Rule, []).

serialize([], [], _Rule, Result) ->
    lists:reverse(Result);
serialize([Value|Values], [Field|Fields], Rule, Result) ->
    case proplists:get_bool(Field, Rule) of
        true when Value =/= undefined -> 
            Data = term_to_binary(Value),
            SerializedValue = base64:encode(Data),
            serialize(Values, Fields, Rule, [SerializedValue|Result]);
        _ ->
            serialize(Values, Fields, Rule, [Value|Result])
    end.

join_fields(Fields) ->
    join_fields(Fields, []).
join_fields([], Result) ->
    binary_string:join(lists:reverse(Result), <<", ">>);
join_fields([Field|Fields], Result) ->
    NewField = list_to_binary([<<"`">>, atom_to_binary(Field, utf8), <<"`">>]),
    join_fields(Fields, [NewField|Result]).

join_values(Values) ->
    join_values(Values, []).
join_values([], Result) ->
    binary_string:join(lists:reverse(Result), <<", ">>);
join_values([Value|Values], Result) -> 
    join_values(Values, [db_fmt:encode(Value)|Result]).
