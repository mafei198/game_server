%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(tcp_acceptor_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link(TransOpts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [TransOpts]).

init([TransOpts]) ->
    {ok, LSocket} = gen_tcp:listen(0, TransOpts),
    AuthSpec = ?CHILD(auth_server, auth_server, worker, []),
    AcceptorSpec = ?CHILD(tcp_acceptor, tcp_acceptor, worker, [LSocket]),
    {ok, {{one_for_one, 10, 10}, [AuthSpec, AcceptorSpec]}}.
