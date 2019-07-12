-module(mail).

-export([send/4, exception_notify/2]).

exception_notify(Category, ExceptionMsg) ->
    {Type, Key} = case Category of
              server -> 
                {"ServerException", exception_mail_receivers};
              client -> 
                {"ClientException", client_exception_mail_receivers}
          end,
    case configuration:get(gs_config, Key) of
        undefined -> ok;
        Receivers ->
            send(os_utils:get_ip_string(), Receivers, Type, ExceptionMsg)
    end.

send(From, Receivers, Subject, Body) ->
    To = lists:foldl(fun(Receiver, Acc) ->
                         if
                             Acc =:= "" ->
                                 Receiver;
                             true ->
                                 lists:concat([Acc, ",", Receiver])
                         end
                         
                     end, "", Receivers),
    Content = io_lib:format("Subject: ~s \r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", 
                            [Subject, From, To, Body]), 
    gen_smtp_client:send({"savin198@163.com", ["savin198@163.com"], Content},
                         [{relay, "smtp.163.com"}, {username, "savin198@163.com"}, {password, "fake_password"}, {ssl, true}, {tls, never}]).
    % gen_smtp_client:send({From, Receivers, Content}, [{relay, "localhost"}, {port, 25}]).
