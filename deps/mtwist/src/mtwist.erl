-module(mtwist).
-export([seed/1, uniform/0, uniform/1,
         new/1, free/1, rand/1, rand/2, rand/3]).
-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, _} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
		      AppPath = filename:dirname(EbinDir),
		      filename:join(AppPath, "priv");
		  Path ->
		      Path
	      end,
    erlang:load_nif(filename:join(PrivDir, "mtwist"), 0).

seed(_X) ->
    exit(nif_library_not_loaded).
uniform() ->
    exit(nif_library_not_loaded).
uniform(N) when is_integer(N) ->
    trunc(N * uniform()).

new(_) ->
    exit(nif_library_not_loaded).
free(_) ->
    exit(nif_library_not_loaded).
rand(_) ->
    exit(nif_library_not_loaded).
rand(State, N) when is_integer(N) ->
    Ratio = rand(State),
    V = trunc(N * Ratio),
    logger:info("RandWithState, N: ~p, Ratio: ~p, V:~p~n", [N, Ratio, V]),
    V.
rand(State, Min, Max) when is_integer(Min) andalso is_integer(Max) andalso Max > Min ->
    trunc(Min + rand(State, Max - Min)).
