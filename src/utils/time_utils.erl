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


-module(time_utils).

-export([now/0,
         days_ago/1,
         one_week/0,
         current_time/0,
         get_now_daynum/0,
         current_month_days/0,
         remain_seconds_to_tomorrow/0,
         begin_of_today/0,
         end_of_today/0,
         end_of_week/0,
         datetime_to_timestamp/1,
         datetime_to_timestamp/2,
         current_time_to_now/1,
         time_to_seconds/3,
         datetime/0,
         datetime_binary/0,
         to_i/1,
         date_number/0,
         date_number/1,
         number_date/1,
         time_string_to_timestamp/1,
         day_diff/2,
         timestamp_to_datetime/1,
         days_seconds/1]).

-define(ORI_SECONDS, 62167219200).

now() ->
    current_time().

days_ago(N) ->
    current_time() - 86400 * N.

one_week() ->
    604800.

current_time() ->
    erlang:system_time(seconds).

% 获取今天星期数
get_now_daynum() ->
    {Date, _} = calendar:universal_time(),
    calendar:day_of_the_week(Date).

% 获取当前月的天数
current_month_days() ->
    {{Year, Month, _}, _} = calendar:universal_time(),
    Days = calendar:last_day_of_the_month(Year, Month),
    {Year, Month, Days}.

remain_seconds_to_tomorrow() ->
    end_of_today() - current_time().

end_of_today() ->
    calendar:datetime_to_gregorian_seconds({date(),{24,0,0}}) - ?ORI_SECONDS.

end_of_week() ->
    DayNum = calendar:day_of_the_week(erlang:date()),
    end_of_today() + (7 - DayNum) * 24 * 3600.

begin_of_today() ->
    calendar:datetime_to_gregorian_seconds({date(),{0,0,0}}) - ?ORI_SECONDS.

datetime_to_timestamp(Date, Time) ->
    calendar:datetime_to_gregorian_seconds({Date, Time}) - ?ORI_SECONDS.

datetime_to_timestamp(Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - ?ORI_SECONDS.

current_time_to_now(CurrentTime) ->
    MegaSecs = CurrentTime div 1000000,
    Secs = CurrentTime rem 1000000,
    {MegaSecs, Secs, 0}.

time_to_seconds(MegaSecs, Secs, _MicroSecs) ->
    MegaSecs * 1000000 + Secs.

datetime() ->
    {datetime, {erlang:date(), erlang:time()}}.

% 2014-07-24 03:07:50
datetime_binary() ->
    {Year, Month, Day} = erlang:date(),
    {Hour, Min, Sec} = erlang:time(),
    List = (integer_to_list(Year) ++ "-" ++ time_number_list(Month) ++ "-" ++ 
            time_number_list(Day) ++ " " ++ time_number_list(Hour) ++ ":" ++ 
            time_number_list(Min) ++ ":" ++ time_number_list(Sec)),
    list_to_binary(List).

to_i({datetime, {Date, Time}}) ->
    calendar:datetime_to_gregorian_seconds({Date, Time})  - ?ORI_SECONDS.

date_number() ->
    date_number(date()).

date_number({Year, Month, Day}) ->
    Year * 10000 + Month * 100 + Day;
date_number(Timestamp) when is_integer(Timestamp) ->
    {Date, _Time} = timestamp_to_datetime(Timestamp),
    date_number(Date).

number_date(Datenumber) ->
    Y = Datenumber div 10000,
    M = Datenumber rem 10000 div 100,
    D = Datenumber rem 100,
    {Y, M, D}.

time_string_to_timestamp(TimeString) ->
    [HourStr, MinutesStr] = binary_string:split(TimeString, <<":">>),
    Hour = binary_to_integer(HourStr),
    Minutes = binary_to_integer(MinutesStr),
    calendar:datetime_to_gregorian_seconds({date(),{Hour, Minutes,0}}) - ?ORI_SECONDS.

timestamp_to_datetime(TimeStamp) ->
    calendar:now_to_datetime(time_utils:current_time_to_now(TimeStamp)).

day_diff(T1, T2) ->
    N1 = current_time_to_now(T1),
    N2 = current_time_to_now(T2),
    {DT1, _} = calendar:now_to_datetime(N1),
    {DT2, _} = calendar:now_to_datetime(N2),
    calendar:date_to_gregorian_days(DT2) - calendar:date_to_gregorian_days(DT1).

days_seconds(Days) ->
    Days * 86400.

time_number_list(Number) ->
    if
        Number < 10 ->
            "0" ++ integer_to_list(Number);
        true ->
            integer_to_list(Number)
    end.
