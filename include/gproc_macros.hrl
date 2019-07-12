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


% Global gproc
%-define(GPROC_KEY(Name), {n, g, Name}).
%-define(GET_PID(Name), gproc:lookup_global_name(Name)).
%-define(REG_PID(Name), gproc:add_global_name(Name)). %% Only can reg yourself.
%-define(UNREG(Name), gproc:unreg(?GPROC_KEY(Name))).

% local gproc
-define(GPROC_KEY(Name), {n, l, Name}).
-define(GET_PID(Name), gproc:where(?GPROC_KEY(Name))).
-define(REG_PID(Name), gproc:reg(?GPROC_KEY(Name))). %% Only can reg yourself.
-define(UNREG(Name), gproc:unreg(?GPROC_KEY(Name))).
-define(SUBSCRIBE(Channel), gproc:reg({p, l, Channel})).
-define(UNSUBSCRIBE(Channel), gproc:unreg({p, l, Channel})).
-define(PUBLISH(Channel, Msg), gproc:send({p, l, Channel}, Msg)).

% Broadcast Channel Msg
-define(BROADCAST(Channel, MsgType, Msg), ?PUBLISH(Channel, {gproc_msg, MsgType, Msg})).

