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


-module(leaderboard_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(LB, test_leaderboard).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?setup(fun tests/1)}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    application:start(gproc),
    application:start(leaderboard),
    {ok, Pid} = leaderboard_sup:start_child([?LB]),
    leaderboard:delete_leaderboard(?LB),
    Pid.

stop(_Pid) ->
    application:stop(leaderboard),
    application:stop(gproc).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
tests(Pid) ->
    times(1, 100, fun(I) ->
        IStr = integer_to_list(I),
        Member = "member:" ++ IStr,
        leaderboard:rank_member(?LB, Member, I),
        MemberData = {"name:" ++ IStr},
        leaderboard:rank_member(?LB, Member, I, MemberData)
    end),
    timer:sleep(30),
    I = 60,
    IStr = integer_to_list(I),
    Member = "member:" ++ IStr,
    NameData = leaderboard:member_data_for(?LB, Member),

    leaderboard:update_member_data(?LB, Member, {undefined}),
    timer:sleep(30),

    UndefinedTuple = leaderboard:member_data_for(?LB, Member),

    leaderboard:remove_member_data(?LB, Member),
    timer:sleep(30),

    UndefinedMemberData = leaderboard:member_data_for(?LB, Member),

    leaderboard:remove_member(?LB, Member),
    timer:sleep(30),

    Ranked = leaderboard:is_member_ranked(?LB, Member),

    leaderboard:rank_members(?LB, [{Member, I}, {"member:101", "101"}]),
    timer:sleep(30),

    TotalMembers = leaderboard:total_members(?LB),
    TotalPages = leaderboard:total_pages(?LB),
    TotalMembersRange = leaderboard:total_members_in_score_range(?LB, 1, 50),

    leaderboard:change_score_for(?LB, Member, 200),
    timer:sleep(30),

    Score = leaderboard:score_for(?LB, Member),
    Rank = leaderboard:rank_for(?LB, Member),
    ScoreAndRank = leaderboard:score_and_rank_for(?LB, Member),

    OldTotalMembers = leaderboard:total_members(?LB),
    leaderboard:remove_members_in_score_range(?LB, 1, 50),
    timer:sleep(30),
    NewTotalMembers = leaderboard:total_members(?LB),

    leaderboard:remove_members_outside_rank(?LB, 50),
    timer:sleep(30),

    OutsideTotalMembers = leaderboard:total_members(?LB),

    TopMemberInfo = leaderboard:top_member(?LB),
    TopMember = hd(tuple_to_list(TopMemberInfo)),
    TopMemberPage = leaderboard:page_for(?LB, TopMember),

    AtMemberInfo = leaderboard:member_at(?LB, 1),

    AroundMeMembers = leaderboard:around_me(?LB, TopMember),

    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual({"name:" ++ "60"}, NameData),
     ?_assertEqual({undefined}, UndefinedTuple),
     ?_assertEqual(undefined, UndefinedMemberData),
     ?_assertEqual(false, Ranked),
     ?_assertEqual(101, TotalMembers),
     ?_assertEqual(5, TotalPages),
     ?_assertEqual(50, TotalMembersRange),
     ?_assertEqual(200 + I, Score),
     ?_assertEqual(1, Rank),
     ?_assertEqual({Score, Rank}, ScoreAndRank),
     ?_assertEqual(OldTotalMembers - 50, NewTotalMembers),
     ?_assertEqual(50, OutsideTotalMembers),
     ?_assertEqual(1, TopMemberPage),
     ?_assertEqual(TopMemberInfo, AtMemberInfo),
     ?_assertEqual(25, length(AroundMeMembers)),
     ?_assertEqual(TopMemberInfo, lists:nth(1, AroundMeMembers))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
times(Max, Max, Fun) -> Fun(Max);
times(I, Max, Fun) -> Fun(I), times(I + 1, Max, Fun).
