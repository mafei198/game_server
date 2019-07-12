-module(misc_utils).

-export([encode_term/1,
         decode_term/1]).

-export([upto/3, upto/4,
         downto/3, downto/4,
         each/2, each/3]).

-export([sort_records/3]).

-export([get_redis_config/0]).

-export([check_name_valid/2]).

-export([map_incr/2,
         map_incr/3]).

encode_term(Term) ->
    base64:encode(term_to_binary(Term)).

decode_term(Base64String) ->
    binary_to_term(base64:decode(Base64String)).

upto(_Fun, From, To) when From > To ->
    undefined;
upto(Fun, From, To) when From =< To->
    case Fun(From) of
        {break, Value} -> Value;
        Value ->
            if
                From =:= To -> Value;
                true -> upto(Fun, From + 1, To)
            end
    end.

upto(_Fun, Acc, From, To) when From > To -> Acc;
upto(Fun, Acc, From, To) when From =< To->
    case Fun(From, Acc) of
        {break, NAcc} -> NAcc;
        NAcc -> 
            if
                From =:= To -> NAcc;
                true -> upto(Fun, NAcc, From + 1, To)
            end
    end.

downto(Fun, From, To) when From >= To->
    case Fun(From) of
        {break, Value} -> Value;
        Value -> 
            if
                From =:= To -> Value;
                true -> downto(Fun, From - 1, To)
            end
    end.

downto(Fun, Acc, From, To) when From >= To->
    case Fun(From, Acc) of
        {break, NAcc} -> NAcc;
        NAcc -> 
            if
                From =:= To -> NAcc;
                true -> downto(Fun, NAcc, From - 1, To)
            end
    end.

each(_Fun, []) -> undefined;
each(Fun, [Elem|Elems]) ->
    case Fun(Elem) of
        {break, Value} -> Value;
        Value -> 
            case Elems of
                [] -> Value;
                _ -> each(Fun, Elems)
            end
    end.

each(_Fun, Acc, []) -> Acc;
each(Fun, Acc, [Elem|Elems]) ->
    case Fun(Elem, Acc) of
        {break, NAcc} -> NAcc;
        NAcc -> each(Fun, NAcc, Elems)
    end.

sort_records(Recs, Field, Order) ->
    lists:sort(fun(A, B) ->
        V1 = record_mapper:get_field(Field, A),
        V2 = record_mapper:get_field(Field, B),
        case Order of
            asc  -> V1 < V2;
            desc -> V2 < V1
        end
    end, Recs).

get_redis_config() ->
    Host = configuration:get(gs_config, redis_host, "127.0.0.1"),
    Port = configuration:get(gs_config, redis_port, 6379),
    DB   = configuration:get(gs_config, redis_db, 0),
    {Host, Port, DB}.

check_name_valid(<<>>, _) -> {fail, error_name_blank};
check_name_valid(Name0, MaxLen) ->
    Name = binary_string:strip(Name0),
    case wordfilter:is_clean(Name) andalso
         binary_string:is_valid_for_mysql(Name) of
        true ->
            case erlang:byte_size(Name) =< MaxLen of
                true -> {ok, Name};
                false -> {fail, error_name_to_long}
            end;
        false -> {fail, error_name_contain_invalid_character}
    end.

map_incr(Key, Map) ->
    map_incr(Key, 1, Map).
map_incr(Key, Incr, Map) ->
    Old = maps:get(Key, Map, 0),
    maps:put(Key, Old + Incr, Map).
