-module(db_fmt).

-export([format/2,
         map/2,
         array/1,
         encode/1]).

map(Fields, Values) ->
    map(Fields, Values, []).

array(Values) ->
    array(Values, []).

map([], [], Result) ->
    binary_string:join(Result, <<", ">>);
map([Field|Fields], [Value|Values], Result) when Value =:= undefined ->
    Map = io_lib:format("`~s` = null", [Field]),
    map(Fields, Values, [list_to_binary(Map)|Result]);
map([Field|Fields], [Value|Values], Result) ->
    Map = io_lib:format("`~s` = ~s", [Field, encode(Value)]),
    map(Fields, Values, [list_to_binary(Map)|Result]).

array([], Result) -> binary_string:join(Result, <<", ">>);
array([Value|Values], Result) ->
    Map = io_lib:format("~s", [encode(Value)]),
    array(Values, [list_to_binary(Map)|Result]).

%% @doc Calls `encode(Val, true)'.
-spec encode(Val::term()) -> binary().
encode(Val) ->
    encode(Val, true).

%% @doc Encode a value as a string or a binary to be embedded in
%% a SQL statement.
%%
%% This function can encode numbers, atoms, date/time/datetime values,
%% strings and binaries (which it escapes automatically).
-spec encode(Val::term(), AsBinary::boolean()) -> string() | binary().
encode(Val, false) when Val =:= undefined; Val =:= null ->
    "null";
encode(Val, true) when Val =:= undefined; Val =:= null ->
    <<"null">>;
encode(Val, false) when is_binary(Val) ->
    binary_to_list(quote(Val));
encode(Val, true) when is_binary(Val) ->
    quote(Val);
encode(Val, true) ->
    list_to_binary(encode(Val,false));
encode(Val, false) when is_atom(Val) ->
    quote(atom_to_list(Val));
encode(Val, false) when is_list(Val) ->
    quote(Val);
encode(Val, false) when is_integer(Val) ->
    integer_to_list(Val);
encode(Val, false) when is_float(Val) ->
    nicedecimal:format(Val);
encode({datetime, Val}, AsBinary) ->
    encode(Val, AsBinary);
encode({{Year,Month,Day}, {Hour,Minute,Second}}, false) ->
    [Year1,Month1,Day1,Hour1,Minute1,Second1] =
        lists:map(fun two_digits/1,[Year, Month, Day, Hour, Minute,Second]),
    lists:flatten(io_lib:format("'~s-~s-~s ~s:~s:~s'",
        [Year1,Month1,Day1,Hour1,Minute1,Second1]));
encode({date, {Year, Month, Day}}, false) ->
    [Year1,Month1,Day1] =
        lists:map(fun two_digits/1,[Year, Month, Day]),
    lists:flatten(io_lib:format("'~s-~s-~s'",[Year1,Month1,Day1]));
encode({time, {Hour, Minute, Second}}, false) ->
    [Hour1,Minute1,Second1] =
        lists:map(fun two_digits/1,[Hour, Minute, Second]),
    lists:flatten(io_lib:format("'~s:~s:~s'",[Hour1,Minute1,Second1]));
encode(Val, _AsBinary) ->
    {error, {unrecognized_value, {Val}}}.

two_digits(Nums) when is_list(Nums) ->
    [two_digits(Num) || Num <- Nums];
two_digits(Num) ->
    [Str] = io_lib:format("~b", [Num]),
    case length(Str) of
        1 -> [$0 | Str];
        _ -> Str
    end.

quote(String) when is_list(String) ->
    [$' | lists:reverse([$' | quote(String, [])])];
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([$\0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([$\n | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([$\r | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([$' | Rest], Acc) ->
    quote(Rest, [$', $\\ | Acc]);
quote([$" | Rest], Acc) ->
    quote(Rest, [$", $\\ | Acc]);
quote([$\^Z | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

format(Formater, Values) ->
    list_to_binary(io_lib:format(Formater, Values)).
