-module(logger).

-export([info/1, info/2]).

info(Msg) ->
    case can_print() of
        true -> error_logger:info_msg(Msg);
        false -> ok
    end.

info(Formater, Args) ->
    case can_print() of
        true -> error_logger:info_msg(Formater, Args);
        false -> ok
    end.

can_print() ->
    case configuration:get(gs_config, server_environment) of
        production ->
            case configuration:get(gs_config, enable_print_debug) of
                true -> true;
                _ -> false
            end;
        _ -> true
    end.
