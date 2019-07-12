-module(exception).

-export([notify/2, 
         notify/3, 
         notify/5, 
         format/2,
         notify_client/2]).

format(Type, Msg) ->
    io_lib:format("~p, ~p~n ~p", [Type, Msg, erlang:get_stacktrace()]).

notify(ErrorType, ErrorMsg) ->
    Stacktrace = format(ErrorType, ErrorMsg),
    do_notify(Stacktrace).

notify(ErrorType, Params, ErrorMsg) ->
    Stacktrace = io_lib:format("~p, ~p, ~p~n ~p", 
                               [Params, ErrorType, ErrorMsg, erlang:get_stacktrace()]),
    do_notify(Stacktrace).

notify(ErrorType, ErrorMsg, Controller, Action, Params) ->
    Stacktrace = io_lib:format("~p, ~p, ~p~n, ~p, ~p~n ~p", 
                               [Controller, Action, Params,
                                ErrorType, ErrorMsg, 
                                erlang:get_stacktrace()]),
    do_notify(Stacktrace).

notify_client(Params, ErrorMsg) ->
    Stacktrace = io_lib:format("~p, ~s~n", 
                               [Params, ErrorMsg]),
    do_notify(client, Stacktrace).

%% 相同异常每1分钟汇报一次
do_notify(Stacktrace) ->
    do_notify(server, Stacktrace).

do_notify(Category, Stacktrace0) ->
    Stacktrace = io_lib:format("[~p]~n ~s", [file:get_cwd(), Stacktrace0]),
    error_logger:error_msg("Caught Exception: ~s~n", [Stacktrace]),
    Id = erlang:crc32(Stacktrace),
    case game_counter:get_daily_action({exception_notify, Id}) of
        0 -> send_notify(Category, Id, Stacktrace);
        LastReportedAt ->
            case configuration:get(gs_config, server_environment) of
                test ->
                    send_notify(Category, Id, Stacktrace);
                _ ->
                    case time_utils:now() - LastReportedAt > 60 of
                        true ->
                            send_notify(Category, Id, Stacktrace);
                        false ->
                            game_counter:incr_daily_action({exception_stored, Id})
                    end
            end
    end.

send_notify(Category, Id, Stacktrace) ->
    _Stored = game_counter:get_daily_action({exception_stored, Id}),
    game_counter:set_daily_action({exception_notify, Id}, time_utils:now()),
    game_counter:del_daily_action({exception_stored, Id}),
    case configuration:get(gs_config, server_environment) of
        production ->
            mail:exception_notify(Category, Stacktrace);
        _ -> do_nothing
    end.

