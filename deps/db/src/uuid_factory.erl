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
%%%        UUID Generator
%%% @end
%%% Created :  二  1 21 13:32:34 2014 by Savin-Max
%%%-------------------------------------------------------------------
-module(uuid_factory).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([gen/0,
         hex/1,
         binary_string_to_objectid/1,
         binary_string_to_objectid/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

gen() ->
    ObjectId = gen_objectid(),
    objectid_to_binary_string(ObjectId).

hex(Binary) ->
    objectid_to_binary_string({Binary}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  OidCounter = case configuration:get(gs_config, server_environment) of
                   test ->
                       % In eunit test after each restart game_server,
                       % the game_server didn't change process id,
                       % so the uuid will conflect with each other, when
                       % executing many eunit test in short time.
                       random:seed(os:timestamp()),
                       random:uniform(10000000);
                   _ -> 0
               end,
  ets:insert (?MODULE, [
    {oid_counter, OidCounter},
    {oid_machineprocid, oid_machineprocid()}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
gen_objectid() ->
  Now = unixtime_to_secs(timenow()),
  MPid = ets:lookup_element(?MODULE, oid_machineprocid, 2),
  N = ets:update_counter(?MODULE, oid_counter, 1),
  objectid(Now, MPid, N).

objectid(UnixSecs, MachineAndProcId, Count) ->
  {<<UnixSecs :32/big, MachineAndProcId :5/binary, Count :24/big>>}.

% Current unixtime to millisecond precision, ie. MicroSecs is always a multiple of 1000.
timenow() -> ms_precision(os:timestamp()).

%@doc Truncate microsecs to millisecs since bson drops microsecs anyway, so time will be equal before and after serialization.
ms_precision ({MegaSecs, Secs, MicroSecs}) ->
  {MegaSecs, Secs, MicroSecs div 1000 * 1000}.

unixtime_to_secs({MegaSecs, Secs, _}) -> MegaSecs * 1000000 + Secs.

-spec oid_machineprocid () -> <<_:40>>. % IO
%@doc Fetch hostname and os pid and compress into a 5 byte id
oid_machineprocid() ->
    OSPid = list_to_integer (os:getpid()),
    {ok, Hostname} = inet:gethostname(),
    <<MachineId:3/binary, _/binary>> = erlang:md5 (Hostname),
    <<MachineId:3/binary, OSPid:16/big>>.

%%--------------------------------------------------------------------
%% @doc:    Generate ObjectId from Binary String
%% @spec:    binary_string_to_objectid(BinaryString::binary()) -> {binary()}.
%% @end
%%--------------------------------------------------------------------
-spec(binary_string_to_objectid(binary()) -> {binary()}).
binary_string_to_objectid(BinaryString) ->
    binary_string_to_objectid(BinaryString, []).

binary_string_to_objectid(<<>>, Result) ->
    {list_to_binary(lists:reverse(Result))};
binary_string_to_objectid(<<BS:2/binary, Bin/binary>>, Result) ->
    binary_string_to_objectid(Bin, [erlang:binary_to_integer(BS, 16)|Result]).

%%--------------------------------------------------------------------
%% @doc:    Generate Binary String from ObjectId
%% @spec:    objectid_to_binary_string(ObjectId::{binary()}) -> binary().
%% @end
%%--------------------------------------------------------------------
-spec(objectid_to_binary_string({binary()}) -> binary()).
objectid_to_binary_string({Id}) ->
    objectid_to_binary_string(Id, []).

objectid_to_binary_string(<<>>, Result) ->
    list_to_binary(lists:reverse(Result));
objectid_to_binary_string(<<Hex:8, Bin/binary>>, Result) ->
    StringList1 = erlang:integer_to_list(Hex, 16),
    StringList2 = case erlang:length(StringList1) of
        1 ->
            ["0"|StringList1];
        _ ->
            StringList1
    end,
    objectid_to_binary_string(Bin, [StringList2|Result]).
