-module(daily_alarm_helper).

-export([start/0, alarm/0, add/2]).

-include("include/db_schema.hrl").


-define(ALARM_KEY, {daily_alarm, is_executed}).

start() -> ok.
    % start_timer(),
    % check_today_executed().

% start_timer() ->
%     After = (time_utils:remain_seconds_to_tomorrow() + 1) * 1000,
%     timer:apply_after(After, daily_alarm_helper, alarm, []).

% check_today_executed() ->
%     DateNumber = game_counter:get(?ALARM_KEY),
%     NowDateNumber = time_utils:date_number(),
%     if
%         DateNumber =:= undefined -> ok;
%         DateNumber < NowDateNumber ->
%             game_counter:set(?ALARM_KEY, NowDateNumber),
%             invoke_callbacks();
%         true -> ok
%     end.

alarm() ->
    % game_counter:set(?ALARM_KEY, time_utils:date_number()),
    % start_timer(),
    invoke_callbacks().

add(Id, MFA) ->
    List = case mochiglobal:get(daily_alarm_callbacks) of
               undefined -> [];
               V -> V
           end,
    case lists:keyfind(Id, 1, List) of
        false ->
            mochiglobal:put(daily_alarm_callbacks, [{Id, MFA}|List]);
        _ ->
            NList = lists:keyreplace(Id, 1, List, {Id, MFA}),
            mochiglobal:put(daily_alarm_callbacks, NList)
    end.

invoke_callbacks() ->
    case mochiglobal:get(daily_alarm_callbacks) of
        undefined -> ok;
        Callbacks -> 
            lists:foreach(fun({_Id, {M, F, A}}) -> 
                safe_execute(fun() -> apply(M, F, A) end)
            end, Callbacks)
    end.

safe_execute(Fun) ->
    try Fun() of
        Result ->
            error_logger:info_msg("Daily Alarm Callback Result: ~p~n", [Result])
    catch
        Type:Msg ->
            exception:notify(Type, daily_alarm_helper, Msg)
    end.
