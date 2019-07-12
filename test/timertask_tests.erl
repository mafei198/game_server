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


-module(timertask_tests).
-include_lib("eunit/include/eunit.hrl").
-export([callback_ok/0]).

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%start_stop_test_() ->
start_stop_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun add_test/1
     ]}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    game_server:start([test]).

stop(_Pid) ->
    game_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
add_test(_Pid) ->
    Key = <<"event_id_1">>,
    AfterSecs = current_time() + 10,
    MFA = {timertask_tests, callback_ok, []},
    timertask:sync_add(Key, AfterSecs, MFA),
    AddResult = timertask:lookup(Key),
    timertask:sync_update(Key, AfterSecs + 10, MFA),
    UpdateResult = timertask:lookup(Key),
    timertask:sync_cancel(Key),
    CancelResult = timertask:lookup(Key),
    [?_assertMatch({Key, _TimerRef, MFA}, AddResult),
     ?_assertMatch({Key, _TimerRef, MFA}, UpdateResult),
     ?_assertEqual(undefined, CancelResult)].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
callback_ok() ->
    callback_ok.

current_time() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
