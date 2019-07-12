-module(leaderboard).

-behaviour(gen_server).

%% API
-export([start_link/4]).

-export([delete_leaderboard/1,
         rank_member/3,
         rank_member/4,
         member_data_for/2,
         update_member_data/3,
         remove_member_data/2,
         rank_members/2,
         remove_member/2,
         total_members/1,
         total_pages/2,
         total_members_in_score_range/3,
         change_score_for/3,
         rank_for/2,
         score_for/2,
         is_member_ranked/2,
         score_and_rank_for/2,
         remove_members_in_score_range/3,
         remove_members_outside_rank/2,
         page_for/3,
         expire_leaderboard/2,
         expire_leaderboard_at/2,
         members/3,
         all_members/1,
         members_from_score_range/3,
         members_from_rank_range/3,
         top_member/1,
         member_at/3,
         around_me/3,
         ranked_in_list/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {redis, name, reverse, page_size}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(LeaderboardName, Host, Port, DB) ->
    gen_server:start_link(?MODULE, [LeaderboardName, Host, Port, DB], []).

delete_leaderboard(Name) ->
    gen_server:cast(leaderboard_pid(Name), {delete_leaderboard}).

rank_member(Name, Member, Score) ->
    gen_server:cast(leaderboard_pid(Name), {rank_member, Member, Score}).

rank_member(Name, Member, Score, MemberData) ->
    gen_server:cast(leaderboard_pid(Name), {rank_member, Member, Score, MemberData}).

member_data_for(Name, Member) ->
    gen_server:call(leaderboard_pid(Name), {member_data_for, Member}).

update_member_data(Name, Member, MemberData) ->
    gen_server:cast(leaderboard_pid(Name), {update_member_data, Member, MemberData}).

remove_member_data(Name, Member) ->
    gen_server:cast(leaderboard_pid(Name), {remove_member_data, Member}).

rank_members(Name, MembersAndScores) ->
    gen_server:cast(leaderboard_pid(Name), {rank_members, MembersAndScores}).

remove_member(Name, Member) ->
    gen_server:cast(leaderboard_pid(Name), {remove_member, Member}).

total_members(Name) ->
    gen_server:call(leaderboard_pid(Name), {total_members}).

total_pages(Name, PageSize) ->
    gen_server:call(leaderboard_pid(Name), {total_pages, PageSize}).

total_members_in_score_range(Name, MinScore, MaxScore) ->
    gen_server:call(leaderboard_pid(Name), 
                    {total_members_in_score_range, MinScore, MaxScore}).

change_score_for(Name, Member, Delta) ->
    gen_server:cast(leaderboard_pid(Name), {change_score_for, Member, Delta}).

rank_for(Name, Member) ->
    gen_server:call(leaderboard_pid(Name), {rank_for, Member}).

score_for(Name, Member) ->
    gen_server:call(leaderboard_pid(Name), {score_for, Member}).

is_member_ranked(Name, Member) ->
    gen_server:call(leaderboard_pid(Name), {is_member_ranked, Member}).

score_and_rank_for(Name, Member) ->
    gen_server:call(leaderboard_pid(Name), {score_and_rank_for, Member}).

remove_members_in_score_range(Name, MinScore, MaxScore) ->
    gen_server:cast(leaderboard_pid(Name), 
                    {remove_members_in_score_range, MinScore, MaxScore}).

remove_members_outside_rank(Name, Rank) ->
    gen_server:cast(leaderboard_pid(Name), {remove_members_outside_rank, Rank}).

page_for(Name, Member, PageSize) ->
    gen_server:call(leaderboard_pid(Name), {page_for, Member, PageSize}).

expire_leaderboard(Name, Seconds) ->
    gen_server:cast(leaderboard_pid(Name), {expire_leaderboard, Seconds}).

expire_leaderboard_at(Name, TimeStamp) ->
    gen_server:cast(leaderboard_pid(Name), {expire_leaderboard_at, TimeStamp}).

members(Name, Page, PageSize) ->
    gen_server:call(leaderboard_pid(Name), {members, Page, PageSize}).

all_members(Name) ->
    gen_server:call(leaderboard_pid(Name), {all_members}).

members_from_score_range(Name, MinScore, MaxScore) ->
    gen_server:call(leaderboard_pid(Name), {members_from_score_range, MinScore, MaxScore}).

members_from_rank_range(Name, MinRank, MaxRank) ->
    gen_server:call(leaderboard_pid(Name), {members_from_rank_range, MinRank, MaxRank}).

top_member(Name) ->
    gen_server:call(leaderboard_pid(Name), {top_member}).

member_at(Name, Position, PageSize) ->
    gen_server:call(leaderboard_pid(Name), {member_at, Position, PageSize}).

around_me(Name, Member, PageSize) ->
    gen_server:call(leaderboard_pid(Name), {around_me, Member, PageSize}).

ranked_in_list(Name, MemberIds) ->
    gen_server:call(leaderboard_pid(Name), {ranked_in_list, MemberIds}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([LeaderboardName, Host, Port, DB]) ->
    gproc:reg({n, l, {leaderboard, LeaderboardName}}),
    {ok, Redis} = eredis:start_link(Host, Port, DB),
    {ok, #state{redis = Redis, 
                name = LeaderboardName,
                reverse = false}}.

handle_call({member_data_for, Member}, _From, State) ->
    MemberData = member_data_for(State#state.redis, State#state.name, Member),
    {reply, MemberData, State};

handle_call({total_members}, _From, State=#state{redis = Redis, name = Name}) ->
    TotalMembers = total_members(Redis, Name),
    {reply, TotalMembers, State};

handle_call({total_pages, PageSize}, _From, State=#state{redis = Redis, name = Name}) ->
    TotalMembers = total_pages(Redis, Name, PageSize),
    {reply, TotalMembers, State};

handle_call({total_members_in_score_range, MinScore, MaxScore}, _From, 
            State=#state{redis = Redis, name = Name}) ->
    TotalMembers = total_members_in_score_range(Redis, Name, MinScore, MaxScore),
    {reply, TotalMembers, State};

handle_call({rank_for, Member}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    Rank = rank_for(Redis, Name, Reverse, Member),
    {reply, Rank, State};

handle_call({score_for, Member}, _From, State=#state{redis = Redis, name = Name}) ->
    Score = score_for(Redis, Name, Member),
    {reply, Score, State};

handle_call({score_and_rank_for, Member}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    ScoreAndRank = score_and_rank_for(Redis, Name, Reverse, Member),
    {reply, ScoreAndRank, State};

handle_call({is_member_ranked, Member}, _From, State=#state{redis = Redis, name = Name}) ->
    IsRanked = is_member_ranked(Redis, Name, Member),
    {reply, IsRanked, State};

handle_call({page_for, Member, PageSize}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    Page = page_for(Redis, Name, Reverse, PageSize, Member),
    {reply, Page, State};

handle_call({members, Page, PageSize}, _From, 
            State=#state{redis = Redis, name = Name, 
                         reverse = Reverse}) ->
    Members = members(Redis, Name, Reverse, PageSize, Page),
    {reply, Members, State};

handle_call({all_members}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    Members = all_members(Redis, Name, Reverse),
    {reply, Members, State};

handle_call({members_from_score_range, MinScore, MaxScore}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    Members = members_from_score_range(Redis, Name, Reverse, MinScore, MaxScore),
    {reply, Members, State};

handle_call({members_from_rank_range, MinRank, MaxRank}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    Members = members_from_rank_range(Redis, Name, Reverse, MinRank, MaxRank),
    {reply, Members, State};

handle_call({top_member}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    Member = top_member(Redis, Name, Reverse),
    {reply, Member, State};

handle_call({member_at, Position, PageSize}, _From, 
            State=#state{redis = Redis, name = Name, 
                         reverse = Reverse}) ->
    Member = member_at(Redis, Name, Reverse, PageSize, Position),
    {reply, Member, State};

handle_call({around_me, Member, PageSize}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    Members = around_me(Redis, Name, Reverse, PageSize, Member),
    {reply, Members, State};

handle_call({ranked_in_list, MemberIds}, _From, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    Members = ranked_in_list(Redis, Name, Reverse, MemberIds),
    {reply, Members, State}.


handle_cast({delete_leaderboard}, State) ->
    delete_leaderboard(State#state.redis, State#state.name),
    {noreply, State};

handle_cast({change_score_for, Member, Delta}, State=#state{redis = Redis, name = Name}) ->
    change_score_for(Redis, Name, Member, Delta),
    {noreply, State};

handle_cast({rank_member, Member, Score}, State) ->
    Redis = State#state.redis, 
    LeaderboardName = State#state.name,
    eredis:q(Redis, ["zadd", LeaderboardName, Score, Member]),
    {noreply, State};

handle_cast({rank_member, Member, Score, MemberData}, State) ->
    Redis = State#state.redis, 
    LeaderboardName = State#state.name,
    transaction(Redis, fun() ->
        eredis:q(Redis, ["zadd", LeaderboardName, Score, Member]),
        eredis:q(Redis, ["hset", member_data_key(LeaderboardName), 
                         Member, encode_member_data(MemberData)])
    end),
    {noreply, State};

handle_cast({update_member_data, Member, MemberData}, State) ->
    Redis = State#state.redis,
    LeaderboardName = State#state.name,
    eredis:q(Redis, ["hset", member_data_key(LeaderboardName), 
                     Member, encode_member_data(MemberData)]),
    {noreply, State};

handle_cast({remove_member_data, Member}, State) ->
    Redis = State#state.redis,
    LeaderboardName = State#state.name,
    eredis:q(Redis, ["hdel", member_data_key(LeaderboardName), Member]),
    {noreply, State};

handle_cast({rank_members, MembersAndScores}, State) ->
    Redis = State#state.redis,
    LeaderboardName = State#state.name,
    transaction(Redis, fun() ->
        lists:foreach(fun({Member, Score}) ->
            eredis:q(Redis, ["zadd", LeaderboardName, Score, Member])
        end, MembersAndScores)
    end),
    {noreply, State};

handle_cast({remove_member, Member}, State) ->
    Redis = State#state.redis,
    LeaderboardName = State#state.name,
    transaction(Redis, fun() ->
        eredis:q(Redis, ["zrem", LeaderboardName, Member]),
        eredis:q(Redis, ["hdel", member_data_key(LeaderboardName), Member])
    end),
    {noreply, State};

handle_cast({remove_members_in_score_range, MinScore, MaxScore}, 
            State=#state{redis = Redis, name = Name}) ->
    remove_members_in_score_range(Redis, Name, MinScore, MaxScore),
    {noreply, State};

handle_cast({remove_members_outside_rank, Rank}, 
            State=#state{redis = Redis, name = Name, reverse = Reverse}) ->
    remove_members_outside_rank(Redis, Name, Reverse, Rank),
    {noreply, State};

handle_cast({expire_leaderboard, Seconds}, 
            State=#state{redis = Redis, name = Name}) ->
    expire_leaderboard(Redis, Name, Seconds),
    {noreply, State};

handle_cast({expire_leaderboard_at, TimeStamp}, 
            State=#state{redis = Redis, name = Name}) ->
    expire_leaderboard_at(Redis, Name, TimeStamp),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

delete_leaderboard(Redis, LeaderboardName) ->
    transaction(Redis, fun() ->
        eredis:q(Redis, ["DEL", LeaderboardName]),
        eredis:q(Redis, ["DEL", member_data_key(LeaderboardName)])
    end).

member_data_for(Redis, LeaderboardName, Member) ->
    case eredis:q(Redis, ["hget", member_data_key(LeaderboardName), Member]) of
        {ok, undefined} -> undefined;
        {ok, Data} -> decode_member_data(Data)
    end.

total_members(Redis, LeaderboardName) ->
    {ok, AmountBin} = eredis:q(Redis, ["zcard", LeaderboardName]),
    binary_to_integer(AmountBin).

total_pages(Redis, LeaderboardName, PageSize) ->
    ceil(total_members(Redis, LeaderboardName) / PageSize).

total_members_in_score_range(Redis, LeaderboardName, MinScore, MaxScore) ->
    {ok, AmountBin} = eredis:q(Redis, ["zcount", LeaderboardName, MinScore, MaxScore]),
    binary_to_integer(AmountBin).

change_score_for(Redis, LeaderboardName, Member, Delta) ->
    eredis:q(Redis, ["zincrby", LeaderboardName, Delta, Member]).

rank_for(Redis, LeaderboardName, Reverse, Member) ->
    case Reverse of
        true ->
            case eredis:q(Redis, ["zrank", LeaderboardName, Member]) of
                {ok, undefined} -> undefined;
                {ok, Rank} -> binary_to_integer(Rank) + 1
            end;
        false ->
            case eredis:q(Redis, ["zrevrank", LeaderboardName, Member]) of
                {ok, undefined} -> undefined;
                {ok, Rank} -> binary_to_integer(Rank) + 1
            end
    end.

score_for(Redis, LeaderboardName, Member) ->
    case eredis:q(Redis, ["zscore", LeaderboardName, Member]) of
        {ok, undefined} -> undefined;
        {ok, Score} -> binary_to_integer(Score)
    end.

is_member_ranked(Redis, LeaderboardName, Member) ->
    case eredis:q(Redis, ["zscore", LeaderboardName, Member]) of
        {ok, undefined} -> false;
        _ -> true
    end.

score_and_rank_for(Redis, LeaderboardName, Reverse, Member) ->
    Results = transaction(Redis, fun() ->
        eredis:q(Redis, ["zscore", LeaderboardName, Member]),
        case Reverse of
            true ->  eredis:q(Redis, ["zrank", LeaderboardName, Member]);
            false -> eredis:q(Redis, ["zrevrank", LeaderboardName, Member])
        end
    end),

    case Results of
        {ok, [Score, Rank]} when is_binary(Score) andalso is_binary(Rank) ->
            {binary_to_integer(Score), binary_to_integer(Rank) + 1};
        {ok, [Score, Rank]} ->
            {Score, Rank}
    end.

remove_members_in_score_range(Redis, LeaderboardName, MinScore, MaxScore) ->
    eredis:q(Redis, ["zremrangebyscore", LeaderboardName, MinScore, MaxScore]).

remove_members_outside_rank(Redis, LeaderboardName, Reverse, Rank) ->
    case Reverse of
        true ->
            eredis:q(Redis, ["zremrangebyrank", LeaderboardName, Rank, -1]);
        false ->
            eredis:q(Redis, ["zremrangebyrank", LeaderboardName, 0, -(Rank) - 1])
    end.

page_for(Redis, LeaderboardName, Reverse, PageSize, Member) ->
    RankForMember = case Reverse of
        true ->
            eredis:q(Redis, ["zrank", LeaderboardName, Member]);
        false ->
            eredis:q(Redis, ["zrevrank", LeaderboardName, Member])
    end,
    case RankForMember of
        {ok, undefined} -> 
            undefined;
        {ok, RankBin} -> 
            Rank = binary_to_integer(RankBin) + 1,
            ceil(Rank / PageSize)
    end.

expire_leaderboard(Redis, LeaderboardName, Seconds) ->
    transaction(Redis, fun() ->
        eredis:q(Redis, ["expire", LeaderboardName, Seconds]),
        eredis:q(Redis, ["expire", member_data_key(LeaderboardName), Seconds])
    end).

expire_leaderboard_at(Redis, LeaderboardName, Timestamp) ->
    transaction(Redis, fun() ->
        eredis:q(Redis, ["expireat", LeaderboardName, Timestamp]),
        eredis:q(Redis, ["expireat", member_data_key(LeaderboardName), Timestamp])
    end).

members(Redis, LeaderboardName, Reverse, PageSize, CurrentPage0) ->
    TotalPages = total_pages(Redis, LeaderboardName, PageSize),
    CurrentPage = lists:min([lists:max([CurrentPage0, 1]), TotalPages]),
    IndexForRedis = CurrentPage - 1,
    StartingOffset = lists:max([IndexForRedis * PageSize, 0]),
    EndingOffset = StartingOffset + PageSize - 1,

    RawLeaderData = case Reverse of
        true ->
            eredis:q(Redis, ["zrange", LeaderboardName, StartingOffset, EndingOffset]);
        false ->
            eredis:q(Redis, ["zrevrange", LeaderboardName, StartingOffset, EndingOffset])
    end,

    case RawLeaderData of
        {ok, []} -> [];
        {ok, Data} -> ranked_in_list(Redis, LeaderboardName, Reverse, Data)
    end.

all_members(Redis, LeaderboardName, Reverse) ->
    RawLeaderData = case Reverse of
        true ->
            eredis:q(Redis, ["zrange", LeaderboardName, 0, -1]);
        false ->
            eredis:q(Redis, ["zrevrange", LeaderboardName, 0, -1])
    end,

    case RawLeaderData of 
        {ok, []} -> [];
        {ok, Data} -> ranked_in_list(Redis, LeaderboardName, Reverse, Data)
    end.

members_from_score_range(Redis, LeaderboardName, Reverse, MinimumScore, MaximumScore) ->
    RawLeaderData = case Reverse of
        true ->
            eredis:q(Redis, ["zrangebyscore", LeaderboardName, MinimumScore, MaximumScore]);
        false ->
            eredis:q(Redis, ["zrevrangebyscore", LeaderboardName, MaximumScore, MinimumScore])
    end,

    case RawLeaderData of
        {ok, []} -> [];
        {ok, Data} -> ranked_in_list(Redis, LeaderboardName, Reverse, Data)
    end.

members_from_rank_range(Redis, LeaderboardName, Reverse, StartingRank0, EndingRank0) ->
    StartingRank = lists:max([StartingRank0 - 1, 0]),

    TotalMembers = total_members(Redis, LeaderboardName),
    EndingRank = lists:min([EndingRank0 - 1, TotalMembers - 1]),

    RawLeaderData = case Reverse of
        true ->
            eredis:q(Redis, ["zrange", LeaderboardName, StartingRank, EndingRank]);
        false ->
            eredis:q(Redis, ["zrevrange", LeaderboardName, StartingRank, EndingRank])
    end,

    case RawLeaderData of
        {ok, []} -> [];
        {ok, Data} -> ranked_in_list(Redis, LeaderboardName, Reverse, Data)
    end.

top_member(Redis, LeaderboardName, Reverse) ->
    case members_from_rank_range(Redis, LeaderboardName, Reverse, 1, 1) of
        [] -> undefined;
        [Member] -> Member
    end.

member_at(Redis, LeaderboardName, Reverse, PageSize, Position) ->
    case Position =< total_members(Redis, LeaderboardName) of
        true ->
            CurrentPage = ceil(Position/ PageSize),
            case members(Redis, LeaderboardName, Reverse, PageSize, CurrentPage) of
                [] -> undefined;
                Leaders -> lists:nth(Position, Leaders)
            end;
        false -> undefined
    end.

around_me(Redis, LeaderboardName, Reverse, PageSize, Member) ->
    ReverseRankForMember = case Reverse of
        true ->
            eredis:q(Redis, ["zrank", LeaderboardName, Member]);
        false ->
            eredis:q(Redis, ["zrevrank", LeaderboardName, Member])
    end,

    case ReverseRankForMember of 
        {ok, undefined} -> [];
        {ok, RankBin} ->
            Rank = binary_to_integer(RankBin),
            StartingOffset0 = Rank - trunc(PageSize / 2),
            StartingOffset = lists:max([StartingOffset0, 0]),
            EndingOffset = StartingOffset + PageSize - 1,
            RawLeaderData = case Reverse of
                true ->
                    eredis:q(Redis, ["zrange", LeaderboardName, 
                                     StartingOffset, EndingOffset]);
                false ->
                    eredis:q(Redis, ["zrevrange", LeaderboardName, 
                                     StartingOffset, EndingOffset])
            end,
            case RawLeaderData of 
                {ok, []} -> [];
                {ok, Data} -> ranked_in_list(Redis, LeaderboardName, Reverse, Data)
            end
    end.

ranked_in_list(Redis, LeaderboardName, Reverse, Members) ->
    {ok, RanksAndScores} = transaction(Redis, fun() ->
        lists:foreach(fun(Member) ->
            case Reverse of
                true ->  eredis:q(Redis, ["zrank", LeaderboardName, Member]);
                false -> eredis:q(Redis, ["zrevrank", LeaderboardName, Member])
            end,
            eredis:q(Redis, ["zscore", LeaderboardName, Member])
        end, Members)
    end),
    if
        RanksAndScores =:= [undefined, undefined] -> [];
        true ->
            ranked_in_list(Members, RanksAndScores, Redis, LeaderboardName, [])
    end.

ranked_in_list([], [], _, _, Result) -> lists:reverse(Result);
ranked_in_list([Member|Members], [Rank, Score|RanksAndScores], 
               Redis, LeaderboardName, Result) ->
    if
        Rank =:= undefined ->
            ranked_in_list(Members, RanksAndScores, Redis, LeaderboardName, Result);
        true ->
            Cell = {Member, binary_to_integer(Rank) + 1, 
                    binary_to_integer(Score), member_data_for(Redis, LeaderboardName, Member)},
            ranked_in_list(Members, RanksAndScores, Redis, LeaderboardName, [Cell|Result])
    end.

transaction(Redis, Fun) ->
    eredis:q(Redis, ["MULTI"]),
    Fun(),
    eredis:q(Redis, ["EXEC"]).

leaderboard_pid(LeaderboardName) ->
    gproc:where({n, l, {leaderboard, LeaderboardName}}).

encode_member_data(Data) ->
    Value = term_to_binary(Data),
    base64:encode(Value).

decode_member_data(Value) ->
    Data = base64:decode(Value),
    binary_to_term(Data).

member_data_key(Name) when is_list(Name) ->
    Name ++ ":" ++ "member_data";
member_data_key(Name) when is_binary(Name) ->
    binary_to_list(Name) ++ ":" ++ "member_data";
member_data_key(Name) when is_atom(Name) ->
    atom_to_list(Name) ++ ":" ++ "member_data".

ceil(X) when X < 0 ->
    trunc(X);
ceil(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.
