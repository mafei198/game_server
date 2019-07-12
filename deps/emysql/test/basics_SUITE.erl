%%%-------------------------------------------------------------------
%%% File     : Emysql/test/basics_SUITE.erl
%%% Descr    : Suite #2: Tests of basic SQL statements,
%%%            prepared statements, stored procedures
%%% Author   : H. Diedrich
%%% Created  : 12/13/2011 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run from Emysql/:
%%%     make test
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-module(basics_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("../include/emysql.hrl").

-record(hello_record, {hello_text}).

%% Optional suite settings
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{seconds,30}}].

%% Mandatory list of test cases and test groups, and skip orders.
%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------

all() ->
    [delete_all,
     encode_atoms,
     insert_only,
     insert_a_record,
     select_a_record,

     insert_and_read_back,
     insert_and_read_back_as_recs,
     insert_and_read_back_as_json,
     select_by_prepared_statement,
	 delete_non_existant_procedure,
	 select_by_stored_procedure,
     multiple_select,
     encode_floating_point_data,
     {group, conversion}].

groups() ->
    [{conversion, [parallel],
        [dict_empty_test,
         dict_single_test,
         dict_multi_test,
         proplist_empty_test,
         proplist_single_test,
         proplist_multi_test,
         json_empty_test,
         json_single_test,
         json_multi_test,
         record_test]}
    ].

%% Optional suite pre test initialization
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------

init_per_suite(Config) ->

	% if this fails, focus on environment_SUITE to fix test setup.
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(test_pool, 1,
        test_helper:test_u(), test_helper:test_p(), "localhost", 3306,
        "hello_database", utf8),

    Config.

%% Optional suite post test wind down
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
	emysql:remove_pool(test_pool),
    ok.

%% A test case. The ok is irrelevant. What matters is, if it returns.
%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------

%% Test Case: Delete all records in the test database
%%--------------------------------------------------------------------
delete_all(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),
    ok.


%% Test Case: Make an Insert
%%--------------------------------------------------------------------
insert_only(_) ->

    emysql:execute(test_pool,
        <<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

    ok.

%TODO: Probably don't need to keep botht his and insert_only
% Test Case: Test if we can insert a record.
%%--------------------------------------------------------------------
insert_a_record(_) ->
    #ok_packet{} = emysql:execute(test_pool, <<"DELETE FROM hello_table">>),
    #ok_packet{} = emysql:execute(test_pool,
        <<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>).

% Test Case: Test if we can select records.
%%--------------------------------------------------------------------
select_a_record(_) ->
    #result_packet{rows=[[<<"Hello World!">>]]} =
    emysql:execute(test_pool, <<"select hello_text from hello_table">>).


%% Test Case: Allow insertion of atom values through the encoder
%%--------------------------------------------------------------------
encode_atoms(_Config) ->
    emysql:execute(test_pool, <<"DROP TABLE encode_atoms_test">>),
    emysql:execute(test_pool, <<"CREATE TABLE encode_atoms_test (x VARCHAR(32))">>),

    emysql:prepare(encode_atoms, <<"INSERT INTO encode_atoms_test (x) VALUES (?)">>),
    Result = emysql:execute(test_pool, encode_atoms, [foo]),
    ct:log("Result: ~p", [Result]),

    ok_packet = element(1, Result),
    ok.

%% Test Case: Make an Insert and Select it back
%%--------------------------------------------------------------------
insert_and_read_back(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

    emysql:execute(test_pool,
        <<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

    Result = emysql:execute(test_pool,
        <<"select hello_text from hello_table">>),

	% find this output by clicking on the test name, then case name in test/index.html
	ct:log("~p~n", [Result]),

	% the test
	{result_packet,5,
               [#field{seq_num=2,
                       catalog= <<"def">>,
                       db= <<"hello_database">>,
                       table= <<"hello_table">>,
                       org_table= <<"hello_table">>,
                       name= <<"hello_text">>,
                       org_name= <<"hello_text">>,
                       type=254,
                       default= <<>>,
                       charset_nr=33,
                       length=_,
                       flags=_,
                       decimals=0,
                       decoder = _ }],
               [[<<"Hello World!">>]],
               <<>>} = Result,

    ok.

%% Test Case: Encode floating point data into a test table (Issue 57)
encode_floating_point_data(_Config) ->
    emysql:execute(test_pool, <<"DROP TABLE float_test">>),
    emysql:execute(test_pool, <<"CREATE TABLE float_test ( x FLOAT )">>),
    emysql:prepare(encode_float_stmt, <<"INSERT INTO float_test (x) VALUES (?)">>),

    Result = emysql:execute(test_pool, encode_float_stmt, [3.14]),

    ct:log("Result: ~p", [Result]),
    ok_packet = element(1, Result),
    ok.

%% Test Case: Make an Insert and Select it back, reading out as Record
%%--------------------------------------------------------------------
insert_and_read_back_as_recs(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	% find this output by clicking on the test name, then case name in test/index.html
	ct:log("~p~n", [Recs]),

	% the test
	Recs = [{hello_record,<<"Hello World!">>}],

	ok.

%% Test Case: Make an Insert and Select it back, reading out as JSON
%%--------------------------------------------------------------------
insert_and_read_back_as_json(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

    emysql:execute(test_pool,
        <<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

    Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

    Recs = emysql_util:as_json(Result),

    % find this output by clicking on the test name, then case name in test/index.html
    io:format("~p~n", [Recs]),

    % the test
    Recs = [[{<<"hello_text">>,<<"Hello World!">>}]],

    ok.

%% Test Case: Create a Prepared Statement and make a Select with it
%%--------------------------------------------------------------------
select_by_prepared_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	emysql:prepare(test_stmt,
		<<"SELECT * from hello_table WHERE hello_text like ?">>),

	Result = emysql:execute(test_pool, test_stmt, ["Hello%"]),

	% find this output by clicking on the test name, then case name in test/index.html
	ct:log("Result: ~p~n", [Result]),

	% the test
	{result_packet,5,
                    [#field{
                            seq_num=2,
                            catalog= <<"def">>,
                            db= <<"hello_database">>,
                            table= <<"hello_table">>,
                            org_table= <<"hello_table">>,
                            name= <<"hello_text">>,
                            org_name= <<"hello_text">>,
                            type=254,
                            default = <<>>,
                            charset_nr = 33,
                            length= _Length,
                            flags = _Flags,
                            decimals = 0,
                            decoder = _Decoder}],
                       [[<<"Hello World!">>]],
                       <<>>} = Result,
    ok.

multiple_select(_) ->
    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	[Result1, Result2] = emysql:execute(test_pool, <<"SELECT * from hello_table; SELECT * from hello_table">>),

	% find this output by clicking on the test name, then case name in test/index.html
	ct:log("Result1: ~p~n", [Result1]),
	ct:log("Result2: ~p~n", [Result2]),

	% the test
	{result_packet,5,
                    [#field{
                            seq_num=_,
                            catalog= <<"def">>,
                            db= <<"hello_database">>,
                            table= <<"hello_table">>,
                            org_table= <<"hello_table">>,
                            name= <<"hello_text">>,
                            org_name= <<"hello_text">>,
                            type=254,
                            default = <<>>,
                            charset_nr = 33,
                            length= _Length,
                            flags = _Flags,
                            decimals = 0,
                            decoder = _Decoder}],
                       [[<<"Hello World!">>]],
                       <<>>} = Result1,
	{result_packet,_,
                    [#field{
                            seq_num=_,
                            catalog= <<"def">>,
                            db= <<"hello_database">>,
                            table= <<"hello_table">>,
                            org_table= <<"hello_table">>,
                            name= <<"hello_text">>,
                            org_name= <<"hello_text">>,
                            type=254,
                            default = <<>>,
                            charset_nr = 33,
                            length= _Length,
                            flags = _Flags,
                            decimals = 0,
                            decoder = _Decoder}],
                       [[<<"Hello World!">>]],
                       <<>>} = Result2,
    ok.

%% Test Case: Delete a non-existant Stored Procedure
%%--------------------------------------------------------------------
delete_non_existant_procedure(_) ->

	Result1 = emysql:execute(test_pool,
	  	<<"drop procedure sp_me_no_exist">>),
	  	% note: returns ok even if sp_hello does not exist

	% find this output by clicking on the test name, then case name in test/index.html
	ct:log("~p~n", [Result1]),

	% test
	Result1 = {error_packet,1,1305,<<"42000">>,
              "PROCEDURE hello_database.sp_me_no_exist does not exist"},

	ok.

%% Test Case: Create a Stored Procedure and make a Select with it
%%--------------------------------------------------------------------
select_by_stored_procedure(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	Result1 = emysql:execute(test_pool,
	  	<<"drop procedure sp_hello">>),
	  	% note: returns ok even if sp_hello does not exist

	% find this output by clicking on the test name, then case name in test/index.html
	ct:log("~p~n", [Result1]),

	% first test
	case Result1 of
		{ok_packet,1,0,0,_,0,[]} -> ok;
		{error_packet,1,1305,<<"42000">>,
              "PROCEDURE hello_database.sp_hello does not exist"} -> ok
    end,

	Result2 = emysql:execute(test_pool,
	  	<<"create procedure sp_hello() begin select * from hello_table limit 2; end">>),

	% find this output by clicking on the test name, then case name in test/index.html
	ct:log("~p~n", [Result2]),

	% second test
	{ok_packet,1,0,0,_,0,[]} = Result2,

	Result3 = emysql:execute(test_pool,	<<"call sp_hello();">>),

	% find this output by clicking on the test name, then case name in test/index.html
	ct:log("~p~n", [Result3]),

	% third, main test
	[{result_packet,5, _Fields, [[<<"Hello World!">>]], <<>>}, {ok_packet,6,0,0,_,0,[]}] = Result3,

	ok.

%%% Conversion routine tests
%% --------------------------------------------------------------------------------------------

dict_empty_test(_) ->
    E = dict:from_list([]),
    E = emysql_util:as_dict(get_empty_test()),
    E = emysql:as_dict(get_empty_test()),
    ok.

dict_single_test(_) ->
    E = dict:from_list([{<<"HelloField">>,<<"Hello">>}]),
    E = emysql_util:as_dict(get_single_test()),
    E = emysql:as_dict(get_single_test()),
    ok.

dict_multi_test(_) ->
    E = dict:from_list([
            {<<"HelloField">>,<<"Hello">>},
            {<<"HiField">>,<<"Hi">>},
            {<<"ByeField">>,<<"Bye">>}
    ]),
    E = emysql_util:as_dict(get_multi_test()),
    E = emysql:as_dict(get_multi_test()),
    ok.

proplist_empty_test(_) ->
    [] = emysql_util:as_proplist(get_empty_test()),
    [] = emysql:as_proplist(get_empty_test()),
    ok.

proplist_single_test(_) ->
    Expect = [ [{<<"HelloField">>,<<"Hello">>}] ],
    Expect = emysql_util:as_proplist(get_single_test()),
    Expect = emysql:as_proplist(get_single_test()),
    ok.

proplist_multi_test(_) ->
    Expect = [ [{<<"HelloField">>,<<"Hello">>},{<<"HiField">>,<<"Hi">>},{<<"ByeField">>,<<"Bye">>}] ],
    Expect = emysql_util:as_proplist(get_multi_test()),
    Expect = emysql:as_proplist(get_multi_test()),
    ok.

json_empty_test(_) ->
    [] = emysql_util:as_json(get_empty_test()),
    [] = emysql:as_json(get_empty_test()),
    ok.

json_single_test(_) ->
    Expect = [ [{<<"HelloField">>,<<"Hello">>}] ],
    Expect = emysql_util:as_json(get_single_test()),
    Expect = emysql:as_json(get_single_test()),
    ok.

json_multi_test(_) ->
    Expect = [ [{<<"HelloField">>,<<"Hello">>},{<<"HiField">>,<<"Hi">>},{<<"ByeField">>,<<"Bye">>}] ],
    Expect = emysql_util:as_json(get_multi_test()),
    Expect = emysql:as_json(get_multi_test()),
    ok.

-record(person, {surname, name, phone, socks}).
record_test(_Config) ->
    emysql:execute(test_pool, <<"DROP TABLE IF EXISTS as_record_test;">>),
    emysql:execute(test_pool, <<"CREATE TABLE as_record_test (name varchar(60), surname varchar(60), socks int)">>),

    emysql:prepare(ins, <<"INSERT INTO as_record_test (name, surname, socks) VALUES (?, ?, ?)">>),
    emysql:execute(test_pool, ins, [<<"Maxim">>, <<"Komar">>, 3]),

                        % test
    Result = emysql:execute(test_pool, <<"select * from as_record_test">>),
    Expected = [#person{ surname = <<"Komar">>, name = <<"Maxim">>, socks=3 }],
    Expected = emysql_util:as_record(Result, person, record_info(fields, person)),
    Expected = emysql:as_record(Result, person, record_info(fields, person)),
    ok.


%%% Data generation
%% --------------------------------------------------------------------------------------------
fields() ->
    [{field,2,<<"def">>,<<"hello_database">>,<<"hello_table">>,
                        <<"hello_table">>,<<"hello_text">>,<<"hello_text">>,
                        254,<<>>,33,60,0,0}].

get_empty_test() ->
    #result_packet {
        seq_num = 5,
        field_list = [#field {
                        seq_num = 2,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"HelloField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 15,
                        flags = 1,
                        decimals = 31 }],
        rows = [],
        extra = <<>>}.

get_single_test() ->
    #result_packet {
        seq_num = 5,
        field_list = [#field {
                        seq_num = 2,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"HelloField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 15,
                        flags = 1,
                        decimals = 31 }],
        rows = [[<<"Hello">>]],
        extra = <<>>}.

get_multi_test() ->
    #result_packet {
        seq_num = 7,
        field_list = [#field {
                        seq_num = 2,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"HelloField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 15,
                        flags = 1,
                        decimals = 31 },
                      #field {
                        seq_num = 3,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"HiField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 6,
                        flags = 1,
                        decimals = 31 },
                      #field {
                        seq_num = 4,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"ByeField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 9,
                        flags = 1,
                        decimals = 31 }],
         rows = [[<<"Hello">>,<<"Hi">>,<<"Bye">>]],
        extra = <<>>}.
