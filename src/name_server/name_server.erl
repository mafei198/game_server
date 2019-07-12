-module(name_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([new_user_name/1,
         get_user_id/1,
         change_user_name/2,
         delete_user_name/1]).

-export([new_name/2,
         get_value/2,
         register_name/3,
         change_name/3,
         delete_name/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_user_name(UserId) ->
    new_name(user_name, UserId).

get_user_id(UserName) ->
    get_value(user_name, UserName).

change_user_name(OldName, NewName) ->
    change_name(user_name, OldName, NewName).

delete_user_name(Name) ->
    delete_name(user_name, Name).


new_name(Prefix, Value) ->
    gen_server:call(?SERVER, {new_name, Prefix, Value}).

get_value(Prefix, Name) ->
    game_counter:get({Prefix, Name}).

register_name(Prefix, Name, Value) ->
    gen_server:call(?SERVER, {register_name, Prefix, Name, Value}).

change_name(Prefix, OldName, NewName) ->
    gen_server:call(?SERVER, {change_name, Prefix, OldName, NewName}).

delete_name(Prefix, Name) ->
    gen_server:cast(?SERVER, {delete_name, Prefix, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({new_name, Prefix, Value}, _From, State) ->
    NewName = gen_name(Prefix, Value),
    {reply, NewName, State};
handle_call({register_name, Prefix, Name, Value}, _From, State) ->
    Success = case game_counter:get({Prefix, Name}) of
                  undefined ->
                      game_counter:set({Prefix, Name}, Value),
                      true;
                  _ -> false
              end,
    {reply, Success, State};
handle_call({change_name, Prefix, OldName, NewName}, _From, State) ->
    Success = case game_counter:get({Prefix, NewName}) of
                  undefined -> 
                      Value = game_counter:get({Prefix, OldName}),
                      game_counter:set({Prefix, NewName}, Value),
                      game_counter:del({Prefix, OldName}),
                      true;
                  _ -> false
              end,
    {reply, Success, State}.

handle_cast({delete_name, Prefix, Name}, State) ->
    game_counter:del({Prefix, Name}),
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

gen_name(Prefix, Value) ->
    Counter = game_counter:incr({Prefix, guest_counter}),
    NewName = list_to_binary([<<"Guest:">>, integer_to_binary(Counter)]),
    case game_counter:get({Prefix, NewName}) of
        undefined -> 
            game_counter:set({Prefix, NewName}, Value),
            NewName;
        _ -> gen_name(Prefix, Value)
    end.
