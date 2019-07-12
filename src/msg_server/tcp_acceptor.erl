-module(tcp_acceptor).

-export([start_link/1]).
-export([loop/1]).

start_link(LSocket) ->
	Pid = spawn_link(?MODULE, loop, [LSocket]),
	{ok, Pid}.

loop(LSocket) ->
    _ = case gen_tcp:accept(LSocket, infinity) of
            {ok, CSocket} ->
                auth_server:add_socket(CSocket);
            %% Reduce the accept rate if we run out of file descriptors.
            %% We can't accept anymore anyway, so we might as well wait
            %% a little for the situation to resolve itself.
            {error, emfile} ->
                receive after 100 -> ok end;
            %% We want to crash if the listening socket got closed.
            {error, Reason} when Reason =/= closed ->
                ok
        end,
    flush(),
    ?MODULE:loop(LSocket).

flush() ->
	receive Msg ->
		error_logger:error_msg(
			"TCP acceptor received unexpected message: ~p~n",
			[Msg]),
		flush()
	after 0 ->
		ok
	end.
