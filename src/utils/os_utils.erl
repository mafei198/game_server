-module(os_utils).

-export([get_ip/0, get_ip_string/0]).

get_ip() ->
    {ok, [{IP, _, _}|_]} = inet:getif(),
    IP.

get_ip_string() ->
    IP = get_ip(),
    io_lib:format("~B.~B.~B.~B", tuple_to_list(IP)).
