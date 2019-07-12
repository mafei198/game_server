-module(redis_generator).

-export([cmd/5]).

-define(MODEL_ORIGIN, 1).
-define(MODEL_UPDATE, 2).
-define(MODEL_DELETE, 3).
-define(MODEL_CREATE, 4).

cmd(Table, Id, Status, Finder, Result) ->
    if
        Status =:= ?MODEL_ORIGIN -> Result;
        Status =:= ?MODEL_CREATE orelse Status =:= ?MODEL_UPDATE ->
            Rec = Finder({Table, Id}),
            cmd(Rec, Status, Result);
        Status =:= ?MODEL_DELETE ->
            cmd({Table, Id}, Status, Result)
    end.

cmd(Rec, ?MODEL_CREATE, Result) ->
    {Table, ForeignKeys, _Fields, Values} = rec_info(Rec),
    redis:gen_create_cmd(list_to_tuple([Table|Values]), ForeignKeys, Result);
cmd(Rec, ?MODEL_UPDATE, Result) ->
    {Table, _ForeignKeys, Fields, Values} = rec_info(Rec),
    Uuid = record_mapper:get_field(uuid, Rec),
    KVList = gen_kvlist(Fields, Values, []),
    redis:gen_update_cmd(Table, Uuid, KVList, Result);
cmd({Table, Uuid}, ?MODEL_DELETE, Result) ->
    ForeignKVList = model:get_foreign_keys_info(Table, Uuid),
    redis:gen_delete_cmd(Table, Uuid, ForeignKVList, Result).

gen_kvlist([], [], Result) -> Result;
gen_kvlist([Field|Fields], [Value|Values], Result) ->
    case Value of
        undefined -> gen_kvlist(Fields, Values, Result);
        _ when is_float(Value) -> 
            gen_kvlist(Fields, Values, [atom_to_list(Field), float_to_list(Value)|Result]);
        _ -> 
            gen_kvlist(Fields, Values, [atom_to_list(Field), Value|Result])
    end.

rec_info(Rec) ->
    [Table|Values] = tuple_to_list(Rec),
    Fields = record_mapper:get_mapping(Table),
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    case erlang:module_loaded(Module) of
        true ->
            Attributes = Module:module_info(attributes),
            Rule = proplists:get_value(serialize, Attributes),
            SerializedValues = serialize(Values, Fields, Rule),
            ForeignKeys = proplists:get_value(foreign_keys, Attributes, []),
            {Table, ForeignKeys, Fields, SerializedValues};
        false ->
            {Table, [], Fields, Values}
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
