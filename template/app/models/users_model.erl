-module (users_model).

-serialize([friend_ids]).

%% framework callback
-export([load_data/1,
         get_player_id/1]).

-export ([info/1,
          current/0]).

-include ("include/db_schema.hrl").
-include ("include/common_const.hrl").
-include ("../app/include/game_const.hrl").

%%%===================================================================
%%% Framework Callbacks
%%%===================================================================

%% Callback: load data from mysql database to Player Process Dict
load_data(PlayerID) ->
    db:find_by(users, uuid, PlayerID).

get_player_id(Udid) ->
    case game_counter:get(Udid) of
        undefined ->
            case db:find_by(users, udid, Udid) of
                {ok, [Rec]} -> Rec#users.uuid;
                {ok, []} -> create_new_user(Udid)
            end;
        Uuid -> Uuid
      end.

create_new_user(Udid) ->
    Uuid = uuid_factory:gen(),
    User = init(Uuid, Udid),
    player:wrap(Uuid, fun() ->
        model:create(User),
        case application:get_env(game_server, server_environment) of
            {ok, test} -> ok;
            _ -> game_counter:set(Udid, Uuid)
        end
    end),
    Uuid.

init(Uuid, Udid) ->
    UserName = name_server:new_user_name(Uuid),
    #users{uuid=Uuid,
           udid=Udid,
           name=UserName}.


%%%===================================================================
%%% API
%%%===================================================================
info(User) ->
    {User#users.uuid,
     User#users.udid,
     User#users.name}.

%% Get current user
current() ->
    PlayerID = get(player_id),
    model:find(#users{uuid=PlayerID}).
