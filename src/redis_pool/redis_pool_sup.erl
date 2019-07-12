-module(redis_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                         permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Args) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(redis_pool, redis_pool, worker, Args)]}}.
