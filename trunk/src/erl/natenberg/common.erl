-module(common).
-export([floor/1, ceiling/1, join/1, sum/2, min/2, max/2]).

% things that should just be in erlang

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

sum(X, Y) ->
	X + Y.

min(X, Y) when X < Y ->
	X;
min(_, Y) ->
  	Y.

max(X, Y) when X > Y ->
	X;
max(_, Y) ->
	Y.

join(List) ->
    join(List, []).
join([], Acc) ->
    Acc;
join([H|T], []) ->
    join(T, H);
join([H|T], Acc) ->
    join(T, lists:append([Acc, ",", H])).