-module(game_counter).

%% data in mneisa with expire time
-export([incr_daily_action/1,
         incr_daily_action/2,
         set_daily_action/2,
         get_daily_action/1,
         del_daily_action/1,
         incr_week_action/1,
         incr_week_action/2,
         get_week_action/1,
         set_expire/3,
         get_expire/1,
         get_expire/2,
         get_expire_with_time/1,
         del_expire/1]).

-export([get/1,
         get/2,
         set/2, 
         del/1, 
         incr/1, 
         incr/2,
         incr_expire/3]).

%% data in ets
-export([set_tmp/2,
         get_tmp/1,
         incr_tmp/1,
         incr_tmp/2]).

-export([clean_expire/0,
         clean_table/1]).

-include("include/ws_defines.hrl").

get(Name, Default) ->
    case game_counter:get(Name) of
        undefined -> Default;
        V -> V
    end.

get(Name) ->
    case shared_data:find(ws_globals, Name) of
        undefined -> undefined;
        Mapper -> Mapper#ws_globals.value
    end.

set(Name, Value) ->
    shared_data:write(#ws_globals{key=Name, value=Value}).

del(Name) ->
    shared_data:delete(ws_globals, Name).

incr(Name) ->
    incr(Name, 1).

incr(Name, Amount) ->
    shared_data:update_counter(ws_globals, Name, Amount).

incr_daily_action(Name) ->
    incr_daily_action(Name, 1).

incr_daily_action(Name, Amount) ->
    incr_expire(Name, Amount, time_utils:end_of_today()).

incr_week_action(Name) ->
    incr_week_action(Name, 1).

incr_week_action(Name, Amount) ->
    incr_expire(Name, Amount, time_utils:end_of_week()).

set_daily_action(Name, Amount) ->
    set_expire(Name, Amount, time_utils:end_of_today()).

get_daily_action(Name) ->
    case get_expire(Name) of
        undefined -> 0;
        V -> V
    end.

del_daily_action(Name) ->
    del_expire(Name).

get_week_action(Name) ->
    get_daily_action(Name).

incr_expire(Key, Incr, ExpireAt) ->
    NValue = case get_expire(Key) of
                 undefined -> 0 + Incr;
                 Value -> Value + Incr
             end,
    set_expire(Key, NValue, ExpireAt),
    NValue.

set_expire(Key, Value, ExpireAt) ->
    Rec = #ws_expire_globals{key = Key, value = Value, expire_at = ExpireAt},
    shared_data:write(Rec).

get_expire(Key) ->
    get_expire(Key, undefined).

get_expire(Key, Default) ->
    case shared_data:find(ws_expire_globals, Key) of
        undefined -> Default;
        Rec ->
            case Rec#ws_expire_globals.expire_at =< time_utils:now() of
                true -> 
                    shared_data:delete(ws_expire_globals, Key),
                    Default;
                false -> Rec#ws_expire_globals.value
            end
    end.

get_expire_with_time(Key) ->
    case shared_data:find(ws_expire_globals, Key) of
        undefined -> undefined;
        Rec ->
            case Rec#ws_expire_globals.expire_at =< time_utils:now() of
                true -> 
                    shared_data:delete(ws_expire_globals, Key),
                    undefined;
                false -> 
                    {Rec#ws_expire_globals.value, Rec#ws_expire_globals.expire_at}
            end
    end.

del_expire(Key) ->
    shared_data:delete(ws_expire_globals, Key).

clean_expire() ->
    Now = time_utils:now(),
    ets:fodl(fun(Key, _Acc) ->
        case shared_data:find(ws_expire_globals, Key) of
            undefined -> ok;
            Rec ->
                case Rec#ws_expire_globals.expire_at =< Now of
                    true -> shared_data:delete(ws_expire_globals, Key);
                    false -> ok
                end
        end
    end, undefined, ws_expire_globals).

clean_table(Tab) ->
    ets:foldl(fun(Rec, _Acc) ->
        shared_data:delete(Tab, element(2, Rec))
    end, undefined, Tab).

set_tmp(Key, Value) ->
    ets:insert(game_tmp_data, #ws_globals{key = Key, value = Value}).

get_tmp(Key) ->
    case ets:lookup(game_tmp_data, Key) of
        [] -> undefined;
        [Rec] -> Rec#ws_globals.value
    end.

incr_tmp(Key) ->
    incr_tmp(Key, 1).
incr_tmp(Key, Amount) ->
    ets:update_counter(game_tmp_data, Key, Amount, {game_tmp_data, Key, 0}).
