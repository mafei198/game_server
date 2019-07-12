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

-module(load_test).
-export([on/1, b/3, bench/2, summary/0, d/1]).

-include("../app/include/secure.hrl").

-define(TAB, ?MODULE).

% -define(IP, "115.29.14.9").
% -define(IP, "104.200.21.189").
-define(IP, "127.0.0.1").
-define(PORT, 5271).

%% C: 并发客户端数量
%% N: 每个客户端发送请求数量
%% I: 客户端请求发送间隔时间

on(C) ->
    b(C, 10000, 1).

d(C) ->
    b(C, 10, 300).

b(C, N, I) ->
    case ets:info(?TAB) of
        undefined -> do_nothing;
        _ -> ets:delete(?TAB)
    end,
    ets:new(?TAB, [set, public, named_table]),
    ets:insert(?TAB, {count, 0}),
    ets:insert(?TAB, {msecs, 0}),
    ets:insert(?TAB, {c, C}),
    ets:insert(?TAB, {n, N}),
    ets:insert(?TAB, {error, 0}),
    ets:insert(?TAB, {number, 0}),
    times(C, fun() -> spawn(load_test, bench, [N, I]) end).

times(0, _F) -> ok;
times(N, F) ->
    F(),
    times(N - 1, F).

bench(N, I) ->
    timer:sleep(trunc(mtwist:uniform(I))),
    Sock = connect(),
    % Counter = ets:update_counter(?TAB, number, 1),
    % UdidStr = io_lib:format("load_test_udid_~p", [Counter]),
    UdidStr = "load_test_udid_" ++ binary_to_list(uuid_factory:gen()),
    Udid = list_to_binary(UdidStr),
    fake_client:send_request(login_params, Sock, {Udid, 1, <<"en">>, 1, <<"">>, <<"guest">>}),

    StartTimeStamp = os:timestamp(),
    run(N, I, Sock, Udid),
    StopTimeStamp = os:timestamp(),
    result(StartTimeStamp, StopTimeStamp),
    exit(normal).

connect() ->
    case gen_tcp:connect(?IP, ?PORT, [{active, false}, {packet, 2}]) of 
        {ok, Socket} -> Socket;
        {error, Reason} -> 
            error_logger:info_msg("connect error: ~p~n", [Reason])
    end.

run(0, _I, Sock, _Udid) -> gen_tcp:close(Sock);
run(N, I, Sock, Udid) ->
    if
        I > 0 ->
            timer:sleep(trunc(mtwist:uniform(I)) + trunc(I/2));
        true ->
            do_nothing
    end,
    fake_client:send_request(formation_info_params, Sock, {1}),
    % fake_client:send_request(login_params, Sock, {Udid, 1, <<"en">>, 1, <<"">>, <<"guest">>}),
    % gen_tcp:send(Sock, "hello, i'm erlang client!!!!!!!!!!!!!!!!!!!"),
    case gen_tcp:recv(Sock, 0) of
        {ok, _Packet} -> 
            _Res = ets:update_counter(?TAB, count, 1),
            % error_logger:info_msg("requested: ~p~n", [Res]),
            run(N-1, I, Sock, Udid);
        Error -> 
            gen_tcp:close(Sock),
            error_logger:info_msg("error: ~p~n", [Error]),
            ets:update_counter(?TAB, error, 1)
    end.

result(StartTimeStamp, StopTimeStamp) ->
    {_StartMegaSecs, StartSecs, StartMicroSecs} = StartTimeStamp,
    {_StopMegaSecs, StopSecs, StopMicroSecs} = StopTimeStamp,
    UsedMicroSecs = StopMicroSecs - StartMicroSecs + (StopSecs - StartSecs) * 1000000,
    ets:update_counter(?TAB, msecs, UsedMicroSecs).
    % io:format("ok~n").

summary() ->
    [{c, C}] = ets:lookup(?TAB, c),
    [{n, N}] = ets:lookup(?TAB, n),
    [{error, Error}] = ets:lookup(?TAB, error),
    [{count, Count}] = ets:lookup(?TAB, count),
    [{msecs, UsedMicroSecs}] = ets:lookup(?TAB, msecs),
    io:format("Used: ~pus~n", [UsedMicroSecs / C]),
    io:format("Used: ~ps~n", [UsedMicroSecs / 1000000 / C]),
    MicroSecsPerRequest = UsedMicroSecs / Count / C,
    io:format("Total Request: ~p~n", [C * N]),
    io:format("Successed Request: ~p~n", [Count]),
    io:format("Error Request: ~p~n", [Error]),
    io:format("MicroSecsPerRequest: ~p~n", [MicroSecsPerRequest]),
    io:format("Requests Per Seconds: ~p~n", [1000000 / MicroSecsPerRequest]).
