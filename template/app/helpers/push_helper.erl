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

-module(push_helper).
-export([push/2, push/3, 
         send_msg/2, batch_send_msg/2]).
-include("include/db_schema.hrl").

push(UserIds, Text) ->
    lists:foreach(fun(UserId) ->
        player:async_wrap(UserId, fun() ->
            User = model:find(#users{uuid = UserId}),
            send_msg(User#users.device_token, Text)
        end)
    end, UserIds).

push(UserIds, PushId, Values) ->
    lists:foreach(fun(UserId) ->
        player:async_wrap(UserId, fun() ->
            User = model:find(#users{uuid = UserId}),
            Text = i18n:t(config_pushes, PushId, User#users.locale, Values),
            send_msg(User#users.device_token, Text)
        end)
    end, UserIds).

send_msg(undefined, _Text) -> ok;
send_msg(DeviceToken, Text) -> 
    apns:send_message(push, binary_to_list(DeviceToken), Text).

batch_send_msg(DeviceTokens, Text) ->
    lists:foreach(fun(DeviceToken) ->
        apns:send_message(push, binary_to_list(DeviceToken), Text)
    end, DeviceTokens).
