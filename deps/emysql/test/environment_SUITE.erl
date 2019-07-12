%%%-------------------------------------------------------------------
%%% File     : Emysql/test/environment_SUITE.erl
%%% Descr    : Suite #1 - testing the test setup, db and pathes =
%%%            availability of crypto app, emysql app and test db. 
%%% Author   : H. Diedrich
%%% Created  : 12/13/2011 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% THIS SUITE DOES NO ACTUAL TESTS BUT CHECKS THE TEST DATABASE ETC.
%%% Test Cases are in this high granularity for clear failure reports.
%%%
%%% Run from Emysql/: 
%%%     make test
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-define(POOL, environment_test_pool).

-module(environment_SUITE).
-include_lib("common_test/include/ct.hrl").

-include("../include/emysql.hrl").

-export([
        all/0,
	init_per_suite/1,
	end_per_suite/1,
        init_per_testcase/2,
        end_per_testcase/2,

        connecting_to_db_and_creating_a_pool_transition/1,

        add_pool_utf8/1,
        add_pool_utf8_deprecated/1,
        add_pool_latin1/1,
        add_pool_latin1_deprecated/1,
        add_pool_latin1_compatible/1,
        add_pool_latin1_compatible_deprecated/1,
        add_pool_time_zone/1,
        add_pool_time_zone_deprecated/1,
        add_pool_wrong_db/1,
        add_pool_wrong_cmd/1,
        add_pool_port_should_be_a_number/1,
        add_pool_size_should_be_a_number/1,
        add_pool_timeout_should_be_a_number/1,
        add_pool_env_defaults/1,
        add_pool_env_all/1

    ]).

% List of test cases.
%%--------------------------------------------------------------------
all() -> 
    [
        connecting_to_db_and_creating_a_pool_transition,

        add_pool_utf8,
        add_pool_utf8_deprecated,
        add_pool_latin1,
        add_pool_latin1_deprecated,
        add_pool_latin1_compatible,
        add_pool_latin1_compatible_deprecated,
        add_pool_time_zone,
        add_pool_time_zone_deprecated,
        add_pool_wrong_db,
        add_pool_wrong_cmd,
        add_pool_port_should_be_a_number,
        add_pool_size_should_be_a_number,
        add_pool_timeout_should_be_a_number,
        add_pool_env_defaults,
        add_pool_env_all
    ].

init_per_suite(Config) ->
    crypto:start(),
    application:start(emysql),
    Config.

end_per_suite(Config) ->
    application:stop(emysql),
    Config.

init_per_testcase(add_pool_env_defaults, Config) ->
    ok = application:stop(emysql),
    ok = application:set_env(emysql, pools, [{?POOL, [
                    {user, test_helper:test_u()},
                    {password, test_helper:test_p()},
                    {host, "localhost"},
                    {port, 3306}
                ]}]
    ),
    ok = application:start(emysql),
    Config;

init_per_testcase(add_pool_env_all, Config) ->
    ok = application:stop(emysql),
    ok = application:set_env(emysql, pools, [{?POOL, [
                    {size, 10},
                    {user, test_helper:test_u()},
                    {password, test_helper:test_p()},
                    {host, "localhost"},
                    {port, 3306},
                    {database, "hello_database"},
                    {encoding, utf8},
                    {start_cmds, [<<"SET TIME_ZONE='+00:00'">>]}
                ]}]
    ),
    ok = application:start(emysql),
    Config;

%TODO: Probably better to split out test suite into ones that expect the pool to exist and 
% tests that are about setting up the pool.

init_per_testcase(_TestCase, Config) ->
    Config.

% If the test created a pool, we should remove it. Note that we should do this even
% For the tests that are supposed to fail, in case they accidentally succeed.

end_per_testcase(_TestCase, _Config) ->  
    application:unset_env(emysql, pools),
    catch emysql:remove_pool(?POOL);

end_per_testcase(_, _) ->
    ok.

%% Test case: test obsolete transitional API
%%--------------------------------------------------------------------
connecting_to_db_and_creating_a_pool_transition(_) ->
    emysql:add_pool(?POOL, [{user,test_helper:test_u()},
			    {password,test_helper:test_p()},
			    {database,"hello_database"},
			    {encoding, utf8}]),
    #result_packet{rows=[[<<"hello_database">>]]} =
	emysql:execute(?POOL, <<"SELECT DATABASE();">>),
    #result_packet{rows=[[<<"utf8">>]]} =
	emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).


add_pool_utf8(_) ->
    emysql:add_pool(?POOL, [{user,test_helper:test_u()}, 
			    {password,test_helper:test_p()},
			    {encoding, utf8}]),
    #result_packet{rows=[[<<"utf8">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

% Note on deprecated tests: keeping while the add_pool/>2 are still part of the api.
add_pool_utf8_deprecated(_) ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(), test_helper:test_p(),
        "localhost", 3306, undefined, utf8),
    #result_packet{rows=[[<<"utf8">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

add_pool_latin1(_) ->
    emysql:add_pool(?POOL, [{user,test_helper:test_u()}, 
			    {password,test_helper:test_p()},
			    {encoding, latin1}]),
    #result_packet{rows=[[<<"latin1">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

add_pool_latin1_deprecated(_) ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(), test_helper:test_p(),
        "localhost", 3306, undefined, latin1),
    #result_packet{rows=[[<<"latin1">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

add_pool_latin1_compatible(_) ->
    emysql:add_pool(?POOL, [{user,test_helper:test_u()}, 
			    {password,test_helper:test_p()}]),
    #result_packet{rows=[[<<"latin1">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

add_pool_latin1_compatible_deprecated(_) ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(), test_helper:test_p(),
        "localhost", 3306, undefined, undefined),
    #result_packet{rows=[[<<"latin1">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).


add_pool_time_zone(_) ->
    emysql:add_pool(?POOL, [{user,test_helper:test_u()}, 
			    {password,test_helper:test_p()},
			    {start_cmds,[<<"SET time_zone='+00:00'">>]}]),
    #result_packet{rows=[[<<"+00:00">>]]} =
    emysql:execute(?POOL, <<"SELECT @@time_zone;">>).

add_pool_time_zone_deprecated(_) ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(), test_helper:test_p(),
        "localhost", 3306, undefined, utf8, [<<"SET time_zone='+00:00'">>]),
    #result_packet{rows=[[<<"+00:00">>]]} =
    emysql:execute(?POOL, <<"SELECT @@time_zone;">>).


add_pool_env_defaults(_) ->
    #result_packet{rows=[[undefined]]} =
    emysql:execute(?POOL, <<"SELECT DATABASE();">>),
    #result_packet{rows=[[<<"latin1">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>),
    #result_packet{rows=[[<<"SYSTEM">>]]} =
    emysql:execute(?POOL, <<"SELECT @@time_zone;">>).

add_pool_env_all(_) ->
    #result_packet{rows=[[<<"hello_database">>]]} =
    emysql:execute(?POOL, <<"SELECT DATABASE();">>),
    #result_packet{rows=[[<<"utf8">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>),
    #result_packet{rows=[[<<"+00:00">>]]} =
    emysql:execute(?POOL, <<"SELECT @@time_zone;">>).

add_pool_wrong_db(_) ->
    {Pid, Mref} = spawn_monitor(fun() ->
                emysql:add_pool(?POOL, 10, test_helper:test_u(),
                    test_helper:test_p(), "localhost", 3306,
                    "this-database-does-not-exist", utf8
                )
        end
    ),
    receive
        {'DOWN', Mref, process, Pid,
            {{nocatch, {failed_to_set_database, _}}, _}} ->
            ok
    after 100 ->
            exit(should_have_failed)
    end,
    % Verify there are no connections added for real
    [] = emysql_conn_mgr:pools().

add_pool_wrong_cmd(_) ->
    {Pid, Mref} = spawn_monitor(fun() ->
                emysql:add_pool(?POOL, [{user,test_helper:test_u()},
					{password,test_helper:test_p()},
					{start_cmds,[<<"syntax error">>]}])
        end
    ),
    receive
        {'DOWN', Mref, process, Pid, {{nocatch, {failed_to_run_cmd, _}}, _}} ->
            ok
    after 100 ->
            exit(should_have_failed)
    end,
    % Verify there are no connections added for real
    [] = emysql_conn_mgr:pools().

add_pool_port_should_be_a_number(_) ->
    {Pid, Mref} = spawn_monitor(fun() ->
                emysql:add_pool(?POOL, [{user,test_helper:test_u()},
					{password,test_helper:test_p()},
					{port,"NotANumber"}])
        end
    ),
    receive
        {'DOWN', Mref, process, Pid, _} ->
            ok;
	_Other ->
	    exit({unexpected,_Other})

    after 100 ->
            exit(should_have_failed)
    end,
    % Verify there are no connections added for real
    [] = emysql_conn_mgr:pools().

add_pool_size_should_be_a_number(_) ->
    {Pid, Mref} = spawn_monitor(fun() ->
                emysql:add_pool(?POOL, [{size,"Ten"}, 
					{user,test_helper:test_u()},
					{password,test_helper:test_p()}])
					
        end
    ),
    receive
        {'DOWN', Mref, process, Pid, _} ->
            ok;
	_Other ->
	    exit({unexpected,_Other})

    after 100 ->
            exit(should_have_failed)
    end,
    % Verify there are no connections added for real
    [] = emysql_conn_mgr:pools().

add_pool_timeout_should_be_a_number(_) ->
    {Pid, Mref} = spawn_monitor(fun() ->
                emysql:add_pool([{user,test_helper:test_u()},
				 {password,test_helper:test_p()},
				 {connect_timeout,"TimeoutNotANumber"}])
        end
    ),
    receive
        {'DOWN', Mref, process, Pid, _} ->
            ok;
	_Other ->
	    exit({unexpected,_Other})

    after 100 ->
            exit(should_have_failed)
    end,
    % Verify there are no connections added for real
    [] = emysql_conn_mgr:pools().
