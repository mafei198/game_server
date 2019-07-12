-module(fcm_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(),string(),tuple()) ->
       {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}.
start_child(Name, ApiKey, ResultCallback) ->
    supervisor:start_child(?MODULE, [Name, ApiKey, ResultCallback]).

-spec init([]) -> {ok, {{supervisor:strategy(), 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(fcm, worker)]}}.
