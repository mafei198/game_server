%%%-------------------------------------------------------------------
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

%%% @doc
%%%        Hold DB pool.
%%%        Provide DB API.
%%% @end
%%% Created :  六 10 12 14:42:45 2013 by Savin-Max
%%%-------------------------------------------------------------------
-module (db).

-behaviour(gen_server).

%% API
-export ([start_link/0,
          init_pool/1,
          create/1,
          delete_by/3,
          delete_all/1,
          update_by/3,
          count_by/3,
          count_all/1,
          page/3,
          find_by/3,
          find_by/4,
          request/2,
          all/1,
          update_all/1,
          execute/1,
          direct_execute/1,
          procedure_name/2,
          execute_with_procedure/2,
          clean_all_procedures/1
         ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (DB_POOL, database_pool).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

create(Record) ->
    [TableName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(TableName),
    Sql = db_fmt:format("INSERT INTO `~s` (~s) VALUES (~s)", 
                        [TableName, db_fmt:map(Fields, Values)]),
    execute(Sql).

delete_by(TableName, Field, Value) ->
    Sql = db_fmt:format("DELETE FROM `~s` WHERE `~s` = ~s", 
                        [TableName, Field, db_fmt:encode(Value)]),
    execute(Sql).

delete_all(TableName) ->
    execute(db_fmt:format("DELETE FROM `~s`", [TableName])).

update_by(Field, Value, Record) ->
    [Table|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Table),
    Sql = db_fmt:format("UPDATE `~s` SET ~s WHERE `~s` = ~s", 
                        [Table, db_fmt:map(Fields, Values), Field, db_fmt:encode(Value)]),
    execute(Sql).

update_all(Record) ->
    [Table|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Table),
    Sql = db_fmt:format("UPDATE `~s` SET ~s", [Table, db_fmt:map(Fields, Values)]),
    execute(Sql).

count_by(Table, Field, Value) ->
    Sql = db_fmt:format("SELECT count(*) FROM `~s` WHERE `~s` = ~s", 
                        [Table, Field, db_fmt:encode(Value)]),
    execute(Sql).

count_all(Table) ->
    Sql = db_fmt:format("SELECT count(*) FROM `~s`", [Table]),
    {result_packet,_,_,[[Count]], _} = execute(Sql),
    Count.

page(Table, Offset, Limit) ->
    Sql = db_fmt:format("SELECT * FROM `~s` limit ~s offset ~s", 
                        [Table, db_fmt:encode(Limit), db_fmt:encode(Offset)]),
    request(Table, Sql).

%% Sqerl = {Field, '=', Value}
%% Sqerl = {{Field, '=', Value}, 'or', {FieldB, '=', ValueB}}
% where(TableName, Sqerl) ->
%     SqlTuple = {select, '*', {from, TableName}, {where, Sqerl}},
%     request(TableName, SqlTuple).

find_by(Table, Field, Value) ->
    Sql = db_fmt:format("SELECT * FROM `~s` WHERE `~s` = ~s", 
                        [Table, Field, db_fmt:encode(Value)]),
    request(Table, Sql).

%% OrderArgs: {created_at, desc}
find_by(Table, Field, Value, {order_by, {OrderField, Arrow}}) ->
    Sql = db_fmt:format("SELECT * FROM `~s` WHERE `~s` = ~s ORDER BY ~s ~s", 
                        [Table, Field, db_fmt:encode(Value), OrderField, Arrow]),
    request(Table, Sql);
find_by(Table, Field, Value, {fields, SelectFields}) ->
    Sql = db_fmt:format("SELECT ~s FROM `~s` WHERE `~s` = ~s",
                        [db_fmt:join_fields(SelectFields), Table, 
                         Field, db_fmt:encode(Value)]),
    request(Table, Sql);
find_by(Table, Field, Value, {limit, Limit}) ->
    Sql = db_fmt:format("SELECT * FROM `~s` WHERE `~s` = ~s LIMIT ~s",
                        [Table, Field, db_fmt:encode(Value), Limit]),
    request(Table, Sql).

all(Table) ->
    Sql = db_fmt:format("SELECT * FROM `~s`", [Table]),
    request(Table, Sql).

request(TableName, Sql) ->
    Res = execute(Sql),
    Fields = record_mapper:get_mapping(TableName),
    {ok, emysql_util:as_record(Res, TableName, Fields)}.

clean_all_procedures(DB) ->
    SQL = db_fmt:format("DELETE FROM mysql.proc WHERE `db` = ~s;", [db_fmt:encode(DB)]),
    direct_execute(SQL).

procedure_name(Name, Suffix) ->
    list_to_binary([Name, <<"_">>, Suffix]).

execute_with_procedure(ProcedureName, Sql) ->
    DropProcedure = list_to_binary([<<"DROP PROCEDURE IF EXISTS ">>, ProcedureName, <<";">>]),
    ExecuteProcedure = list_to_binary([<<"CALL ">>, ProcedureName, <<"();">>]),
    ProcedureSQL = list_to_binary([DropProcedure,
                                   <<"CREATE PROCEDURE ">>, ProcedureName, <<"()">>,
                                   <<" BEGIN ">>,
                                   <<" DECLARE exit handler for sqlexception ">>,
                                   <<" BEGIN ">>,
                                   <<" ROLLBACK; ">>,
                                   <<" RESIGNAL; ">>,
                                   <<" END; ">>,
                                   <<" DECLARE exit handler for sqlwarning ">>,
                                   <<" BEGIN ">>,
                                   <<" ROLLBACK; ">>,
                                   <<" RESIGNAL; ">>,
                                   <<" END; ">>,
                                   <<" START TRANSACTION; ">>,
                                   Sql, <<";">>,
                                   <<" COMMIT; ">>,
                                   <<" END ">>, <<";">>,
                                   ExecuteProcedure,
                                   DropProcedure]),
    % error_logger:info_msg("EXECUTE PROCEDURE FOR [~p]: ~p~n", 
    %                       [get(player_id), CreateProcedure]),
    % db:execute(ProcedureSQL).
    Results = emysql:execute(?DB_POOL, ProcedureSQL),
    lists:foreach(fun(Result) ->
        case is_tuple(Result) of
            true ->
                case element(1, Result) of
                    error_packet -> 
                        erlang:error(Result);
                    _ -> ok
                end;
            false -> ok
        end
    end, Results).

%%--------------------------------------------------------------------
%% @doc:    Execute SQL and return Result
%% @spec:    execute(SQL::binary()) -> list().
%% @end
%%--------------------------------------------------------------------
-spec(execute(binary()) -> tuple() ).
execute(SQL) ->
    case gen_server:call(?SERVER, {queue_execute, SQL}) of
        {ok, Reply} -> Reply;
        {exception, Type, Msg} ->
            error({Type, Msg}, [SQL])
    end.

%%%===================================================================
%%% gen_server API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init_pool(Config) ->
    gen_server:call(?SERVER, {init_pool, Config}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({queue_execute, SQL}, _From, State) ->
    Reply = try direct_execute(SQL) of
                R -> {ok, R}
            catch
                Type:Msg ->
                    {exception, Type, Msg}
            end,
    {reply, Reply, State};
handle_call({init_pool, Config}, _From, State) ->
    Pool = do_init_pool(Config),
    Database = proplists:get_value("database", Config),
    clean_all_procedures(Database),
    {reply, Pool, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    remove_pool(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init_pool(L) ->
    Database = proplists:get_value("database", L),
    Username = proplists:get_value("username", L),
    Password = proplists:get_value("password", L),
    Encoding = list_to_atom(proplists:get_value("encoding", L)),
    PoolSize = proplists:get_value("pool",L),
    Host = case proplists:get_value("host", L) of
               undefined -> "localhost";
               H -> H
           end,
    error_logger:info_msg("Database: ~p, Username: ~p, Password: ~p, Encoding: ~p, PoolSize: ~p, Host: ~p~n", 
                          [Database, Username, Password, Encoding, PoolSize, Host]),
    ok = emysql:add_pool(?DB_POOL, PoolSize, Username, Password,
                         Host, 3306, Database, Encoding).

remove_pool() ->
    ok = emysql:remove_pool(?DB_POOL).

direct_execute(SQL) ->
    % error_logger:info_msg("SQL: ~p~n", [SQL]),
    Result = emysql:execute(?DB_POOL, SQL),
    % error_logger:info_msg("SQL EXECUTE RESULT: ~p~n", [Result]),
    case is_tuple(Result) of
        true ->
            case tuple_to_list(Result) of
                [error_packet|_] ->
                    erlang:error(Result);
                _ ->
                    Result
            end;
        false ->
            Result
    end.
