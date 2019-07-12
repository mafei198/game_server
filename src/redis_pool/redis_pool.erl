-module(redis_pool).

-behaviour(gen_server).

%% API
-export([start_link/3, 
         get_redis/1,
         del_redis/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {host, port, db}).

-include("include/gproc_macros.hrl").


%%%===================================================================
%%% API
%%%===================================================================
start_link(Host, Port, DB) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port, DB], []).

get_redis(Name) ->
    gen_server:call(?SERVER, {get_redis, Name}).

del_redis(Name) ->
    gen_server:cast(?SERVER, {del_redis, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port, DB]) ->
    {ok, #state{host = Host, port = Port, db = DB}}.

handle_call({get_redis, Name}, _From, State) ->
    Instance = case get({redis, Name}) of
        undefined ->
            {ok, Redis} = eredis:start_link(State#state.host, State#state.port, State#state.db),
            put({redis, Name}, Redis),
            Redis;
        Redis -> Redis
    end,
    {reply, Instance, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({del_redis, Name}, State) ->
    case get({redis, Name}) of
        undefined -> ok;
        Redis -> 
            eredis:stop(Redis),
            erase({redis, Name})
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
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

