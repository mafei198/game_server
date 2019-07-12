-module(number).

-export([floor/1, ceil/1, fix_float/1]).

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).


ceil(X) when X < 0 ->
    trunc(X);
ceil(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

fix_float(Float) when Float < 1 andalso Float > -1 -> Float;
fix_float(Float) ->
    Diff = Float - trunc(Float),
    if
        Diff <  1 andalso Diff >= 0.99 ->
            trunc(Float) + 1;
        Diff < -1 andalso Diff =< -0.99 ->
            trunc(Float) - 1;
        true ->
            trunc(Float)
    end.
