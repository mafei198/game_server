-module(shared_data_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-include("../app/include/game_const.hrl").

-define(SERVER, ?MODULE).
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                         permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(shared_data, shared_data, worker, [])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

