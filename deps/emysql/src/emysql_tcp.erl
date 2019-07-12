%% Copyright (c) 2009-2011
%% Bill Warnecke <bill@rupture.com>,
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>,
%% Henning Diedrich <hd2010@eonblast.com>,
%% Eonblast Corporation <http://www.eonblast.com>
%%
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy,
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following
%% conditions:
%%
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% @private
-module(emysql_tcp).
-export([send_and_recv_packet/3, recv_packet/3, parse_response/4]).

-include("emysql.hrl").
-include("emysql_internal.hrl").

-type packet_result() :: #eof_packet{} | #ok_packet{} | #result_packet{} | #error_packet{}.

-spec send_and_recv_packet(port(), iodata(), integer()) -> packet_result() | [packet_result()].
send_and_recv_packet(Sock, Packet, SeqNum) ->
    case gen_tcp:send(Sock, [<<(size(Packet)):24/little, SeqNum:8>>, Packet]) of
        ok -> ok;
        {error, closed} ->
            %% If we can't communicate on the socket since it has been closed, we exit the process
            %% at this point. The exit reason is caught by `emysql:monitor_work/3` and since it is
            %% with the atom `conn_tcp_closed` we special-case that and rehandle it properly
            exit(tcp_connection_closed)
    end,
    DefaultTimeout = emysql_app:default_timeout(),
    case response_list(Sock, DefaultTimeout, ?SERVER_MORE_RESULTS_EXIST) of
        % This is a bit murky. It's compatible with former Emysql versions
        % but sometimes returns a list, e.g. for stored procedures,
        % since an extra OK package is sent at the end of their results.
        [Record] -> Record;
        List -> List
    end.

response_list(Sock, DefaultTimeout, ServerStatus) -> 
    response_list(Sock, DefaultTimeout, ServerStatus, <<>>).

response_list(_, _DefaultTimeout, 0, <<>>) -> [];  %%no further data received after last response.

response_list(Sock, DefaultTimeout, ?SERVER_MORE_RESULTS_EXIST, Buff) ->
    {Packet, Rest} = recv_packet(Sock, DefaultTimeout, Buff),
    {Response, ServerStatus, Rest2} = parse_response(Sock, DefaultTimeout, Packet, Rest),
    [ Response | response_list(Sock, DefaultTimeout, ServerStatus band ?SERVER_MORE_RESULTS_EXIST, Rest2)].



recv_packet(Sock, DefaultTimeout, Buff) ->
    {PacketLength, SeqNum, Buff2} = recv_packet_header(Sock, DefaultTimeout, Buff),
    {Data, Rest} = recv_packet_body(Sock, PacketLength, DefaultTimeout, Buff2),
    {#packet{size=PacketLength, seq_num=SeqNum, data=Data}, Rest}.

parse_response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<0:8, Rest/binary>>}=_Packet, Buff) ->
    {AffectedRows, Rest1} = lcb(Rest),
    {InsertId, Rest2} = lcb(Rest1),
    <<ServerStatus:16/little, WarningCount:16/little, Msg/binary>> = Rest2,
    { #ok_packet{
        seq_num = SeqNum,
        affected_rows = AffectedRows,
        insert_id = InsertId,
        status = ServerStatus,
        warning_count = WarningCount,
        msg = unicode:characters_to_list(Msg) },
      ServerStatus, Buff };

% EOF: MySQL format <= 4.0, single byte. See -2-
parse_response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<?RESP_EOF:8>>}=_Packet, Buff) ->
    { #eof_packet{
        seq_num = SeqNum },
      ?SERVER_NO_STATUS, Buff };

% EOF: MySQL format >= 4.1, with warnings and status. See -2-
parse_response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<?RESP_EOF:8, WarningCount:16/little, ServerStatus:16/little>>}=_Packet, Buff) ->
    { #eof_packet{
        seq_num = SeqNum,
        status = ServerStatus,
        warning_count = WarningCount },
      ServerStatus, Buff };

% ERROR response: MySQL format >= 4.1. See -3-
parse_response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<255:8, ErrNo:16/little, "#", SQLState:5/binary-unit:8, Msg/binary>>}=_Packet, Buff) ->
    { #error_packet{
        seq_num = SeqNum,
        code = ErrNo,
        status = SQLState,
        msg = binary_to_list(Msg) }, % todo: test and possibly conversion to UTF-8
     ?SERVER_NO_STATUS, Buff };

% ERROR response: MySQL format <= 4.0. See -3-
parse_response(_Sock, _Timeout, #packet{seq_num = SeqNum, data = <<255:8, ErrNo:16/little, Msg/binary>>}=_Packet, Buff) ->
    { #error_packet{
        seq_num = SeqNum,
        code = ErrNo,
        status = <<0>>,
        msg = binary_to_list(Msg) }, % todo: test and possibly conversion to UTF-8
     ?SERVER_NO_STATUS, Buff };

% DATA response.
parse_response(Sock, DefaultTimeout, #packet{seq_num = SeqNum, data = Data}=_Packet, Buff) ->
    {FieldCount, Rest1} = lcb(Data),
    {Extra, _} = lcb(Rest1),
    {SeqNum1, FieldList, Buff2} = recv_field_list(Sock, SeqNum+1, DefaultTimeout, Buff),
    if
        length(FieldList) =/= FieldCount ->
            exit(query_returned_incorrect_field_count);
        true ->
            ok
    end,
    {SeqNum2, Rows, ServerStatus, Buff3} = recv_row_data(Sock, FieldList, DefaultTimeout, SeqNum1+1, Buff2),
    { #result_packet{
        seq_num = SeqNum2,
        field_list = FieldList,
        rows = Rows,
        extra = Extra },
      ServerStatus, Buff3 }.

recv_packet_header(_Sock, _Timeout, <<PacketLength:24/little-integer, SeqNum:8/integer, Rest/binary>>) ->
        {PacketLength, SeqNum, Rest};
recv_packet_header(Sock, Timeout, Buff) when erlang:byte_size(Buff) < 4 ->
        case gen_tcp:recv(Sock, 0, Timeout) of
            {ok, Data} ->
                recv_packet_header(Sock, Timeout, <<Buff/binary, Data/binary>>);
            {error, Reason} ->
                exit({failed_to_recv_packet_header, Reason})
        end;
recv_packet_header(_Sock, _Timeout, Buff) ->
        exit({bad_packet_header_data, Buff}).
    

recv_packet_body(Sock, PacketLength, Timeout, Buff) ->
    case Buff of
        <<Bin:PacketLength/binary, Rest/binary>> ->
            {Bin, Rest};
        _ when erlang:byte_size(Buff) < PacketLength ->
            case gen_tcp:recv(Sock, 0, Timeout) of
                    {ok, Bin} ->
                        recv_packet_body(Sock, PacketLength , Timeout, <<Buff/binary, Bin/binary>>);
                    {error, Reason1} ->
                        exit({failed_to_recv_packet_body, Reason1})
            end
    end.

recv_field_list(Sock, SeqNum, DefaultTimeout, Buff) ->
    recv_field_list(Sock, SeqNum, DefaultTimeout,[], Buff).

recv_field_list(Sock, _SeqNum, DefaultTimeout, Acc, Buff) ->
	case recv_packet(Sock, DefaultTimeout, Buff) of
        {#packet{seq_num = SeqNum1, data = <<?RESP_EOF, _WarningCount:16/little, _ServerStatus:16/little>>}, Unparsed} ->
                        {SeqNum1, lists:reverse(Acc), Unparsed};
        {#packet{seq_num = SeqNum1, data = <<?RESP_EOF, _/binary>>}, Unparsed} ->
                        {SeqNum1, lists:reverse(Acc), Unparsed};
        {#packet{seq_num = SeqNum1, data = Data}, Unparsed} ->
			{Catalog, Rest2} = lcs(Data),
			{Db, Rest3} = lcs(Rest2),
			{Table, Rest4} = lcs(Rest3),
			{OrgTable, Rest5} = lcs(Rest4),
			{Name, Rest6} = lcs(Rest5),
			{OrgName, Rest7} = lcs(Rest6),
			<<_:1/binary, CharSetNr:16/little, Length:32/little, Rest8/binary>> = Rest7,
			<<Type:8/little, Flags:16/little, Decimals:8/little, _:2/binary, Rest9/binary>> = Rest8,
			{Default, _} = lcb(Rest9),
			Field = #field{
				seq_num = SeqNum1,
				catalog = Catalog,
				db = Db,
				table = Table,
				org_table = OrgTable,
				name = Name,
				org_name = OrgName,
				type = Type,
				default = Default,
				charset_nr = CharSetNr,
				length = Length,
				flags = Flags,
				decimals = Decimals,
 				decoder = cast_fun_for(Type)
			},
			recv_field_list(Sock, SeqNum1, DefaultTimeout, [Field|Acc], Unparsed)
	end.


recv_row_data(Socket, FieldList, DefaultTimeout, SeqNum, Buff) ->
    recv_row_data(Socket, FieldList, DefaultTimeout, SeqNum, Buff, []).

recv_row_data(Socket, FieldList, Timeout, SeqNum, Buff, Acc) ->
       case parse_buffer(FieldList,Buff, Acc) of
                {ok, NotParsed, NewAcc, Missing} ->
                    case gen_tcp:recv(Socket, Missing, Timeout) of
                        {ok, Data} ->
                            recv_row_data(Socket, FieldList, Timeout, SeqNum+1,  <<NotParsed/binary, Data/binary>>, NewAcc);
                        {error, Reason} ->
                            exit({failed_to_recv_row, Reason})
                    end;
                {eof, Seq, NewAcc, ServerStatus, NotParsed} ->
                    {Seq, lists:reverse(NewAcc), ServerStatus, NotParsed}
        end.

parse_buffer(FieldList,<<PacketLength:24/little-integer, SeqNum:8/integer, PacketData:PacketLength/binary, Rest/binary>>, Acc) ->
    case PacketData of
        <<?RESP_EOF, _WarningCount:16/little, ServerStatus:16/little>> ->
            {eof, SeqNum, Acc, ServerStatus, Rest};
        <<?RESP_EOF, _/binary>> ->
            {eof, SeqNum, Acc, ?SERVER_NO_STATUS, Rest};
        _ ->
            Row = decode_row_data(PacketData, FieldList),
            parse_buffer(FieldList,Rest, [Row|Acc])
    end;
parse_buffer(_FieldList, Buff = <<PacketLength:24/little-integer, _SeqNum:8/integer, PacketData/binary>>, Acc) ->
    Missing = PacketLength - size(PacketData),
    if
        Missing =< ?TCP_RECV_BUFFER ->
            {ok, Buff, Acc, 0};
        true ->
            {ok, Buff, Acc, Missing}
    end;
parse_buffer(_FieldList,Buff, Acc) ->
    {ok, Buff, Acc, 0}.

decode_row_data(<<>>, []) ->
    [];
decode_row_data(<<Length:8, Data:Length/binary, Tail/binary>>, [Field|Rest]) 
        when Length =< 250 ->
    [type_cast_row_data(Data, Field) | decode_row_data(Tail, Rest)];
%% 251 means null
decode_row_data(<<251:8, Tail/binary>>, [Field|Rest]) ->  
    [type_cast_row_data(undefined, Field) | decode_row_data(Tail, Rest)];
decode_row_data(<<252:8, Length:16/little, Data:Length/binary, Tail/binary>>, [Field|Rest]) ->
    [type_cast_row_data(Data, Field) | decode_row_data(Tail, Rest)];
decode_row_data(<<253:8, Length:24/little, Data:Length/binary, Tail/binary>>, [Field|Rest]) ->
    [type_cast_row_data(Data, Field) | decode_row_data(Tail, Rest)];
decode_row_data(<<254:8, Length:64/little, Data:Length/binary, Tail/binary>>, [Field|Rest]) ->
    [type_cast_row_data(Data, Field) | decode_row_data(Tail, Rest)].

cast_fun_for(Type) ->
    Map = [{?FIELD_TYPE_VARCHAR, fun identity/1},
     {?FIELD_TYPE_TINY_BLOB, fun identity/1},
     {?FIELD_TYPE_MEDIUM_BLOB, fun identity/1},
     {?FIELD_TYPE_LONG_BLOB, fun identity/1},
     {?FIELD_TYPE_BLOB, fun identity/1},
     {?FIELD_TYPE_VAR_STRING, fun identity/1},
     {?FIELD_TYPE_STRING, fun identity/1},
     {?FIELD_TYPE_TINY, fun to_integer/1},
     {?FIELD_TYPE_SHORT, fun to_integer/1},
     {?FIELD_TYPE_LONG, fun to_integer/1},
     {?FIELD_TYPE_LONGLONG, fun to_integer/1},
     {?FIELD_TYPE_INT24, fun to_integer/1},
     {?FIELD_TYPE_YEAR, fun to_integer/1},
     {?FIELD_TYPE_DECIMAL, fun to_float/1},
     {?FIELD_TYPE_NEWDECIMAL, fun to_float/1},
     {?FIELD_TYPE_FLOAT, fun to_float/1},
     {?FIELD_TYPE_DOUBLE, fun to_float/1},
     {?FIELD_TYPE_DATE, fun to_date/1},
     {?FIELD_TYPE_TIME, fun to_time/1},
     {?FIELD_TYPE_TIMESTAMP, fun to_timestamp/1},
     {?FIELD_TYPE_DATETIME, fun to_timestamp/1},
     {?FIELD_TYPE_BIT, fun to_bit/1}
    ],
% TODO:
% ?FIELD_TYPE_NEWDATE
% ?FIELD_TYPE_ENUM
% ?FIELD_TYPE_SET
% ?FIELD_TYPE_GEOMETRY
    case lists:keyfind(Type, 1, Map) of
        false ->
            fun identity/1;
        {Type, F} ->
            F
    end.

identity(Data) -> Data.
to_integer(Data) -> list_to_integer(binary_to_list(Data)).
to_float(Data) ->
    {ok, [Num], _Leftovers} = case io_lib:fread("~f", binary_to_list(Data)) of
                                           % note: does not need conversion
        {error, _} ->
          case io_lib:fread("~d", binary_to_list(Data)) of  % note: does not need conversion
            {ok, [_], []} = Res ->
              Res;
            {ok, [X], E} ->
              io_lib:fread("~f", lists:flatten(io_lib:format("~w~s~s" ,[X,".0",E])))
          end
        ;
        Res ->
          Res
    end,
    Num.
to_date(Data) ->
    case io_lib:fread("~d-~d-~d", binary_to_list(Data)) of  % note: does not need conversion
        {ok, [Year, Month, Day], _} ->
            {date, {Year, Month, Day}};
        {error, _} ->
            binary_to_list(Data);  % todo: test and possibly conversion to UTF-8
        _ ->
            exit({error, bad_date})
    end.
to_time(Data) ->
    case io_lib:fread("~d:~d:~d", binary_to_list(Data)) of  % note: does not need conversion
        {ok, [Hour, Minute, Second], _} ->
            {time, {Hour, Minute, Second}};
        {error, _} ->
            binary_to_list(Data);  % todo: test and possibly conversion to UTF-8
        _ ->
            exit({error, bad_time})
    end.
to_timestamp(Data) ->
    case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Data)) of % note: does not need conversion
        {ok, [Year, Month, Day, Hour, Minute, Second], _} ->
            {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
        {error, _} ->
            binary_to_list(Data);   % todo: test and possibly conversion to UTF-8
        _ ->
            exit({error, datetime})
    end.
to_bit(<<1>>) -> 1;
to_bit(<<0>>) -> 0.

type_cast_row_data(undefined, _) -> undefined;
type_cast_row_data(Data, #field{decoder = F}) -> F(Data).

%% lcb/1 decodes length-coded-integer values
lcb(<<>>) -> {<<>>, <<>>}; % This clause should be removed when we have control
lcb(<< Value:8, Rest/bits >>) when Value =< 250 -> {Value, Rest};
lcb(<< 252:8, Value:16/little, Rest/bits >>) -> {Value, Rest};
lcb(<< 253:8, Value:24/little, Rest/bits >>) -> {Value, Rest};
lcb(<< 254:8, Value:64/little, Rest/bits >>) -> {Value, Rest}.

%% lcs/1 decodes length-encoded-string values
lcs(<< 251:8, Rest/bits >>) -> {undefined, Rest};
lcs(Bin) ->
    {Length, Rest} = lcb(Bin),
    << String:Length/binary, Excess/binary>> = Rest,
    {String, Excess}.
