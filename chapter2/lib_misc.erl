-module(lib_misc).
-import(lists, [seq/2]).
-export([qsort/1, pythag/1, perms/1, odds_and_evens/1, odds_and_evens2/1]).
qsort([]) -> [];
qsort([Pivot|T]) -> 
    qsort([X || X <- T, X < Pivot]) ++ [Pivot] ++
    qsort([X || X <- T, X > Pivot]).

pythag(N) -> 
    [{A, B, C} || 
        A <- seq(1, N),
        B <- seq(1, N),
        C <- seq(1, N),
        A + B + C =< N,
        A*A + B*B =:= C*C
    ].

perms([]) -> [[]];
perms(L) -> 
    [[H|T] || 
        H <- L,
        T <- perms(L--[H])
    ].

odds_and_evens(L) ->
    Odds = [X || X <- L, (X rem 2) =:= 0],
    Evens = [X || X <- L, (X rem 2) =:= 1],
    {Odds, Evens}.

odds_and_evens2(L) ->
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
    case (H rem 2) of 
        0 -> odds_and_evens_acc(T, [H|Odds], Evens);
        1 -> odds_and_evens_acc(T, Odds, [H|Evens])
    end;
odds_and_evens_acc([], Odds, Evens) ->
    {Odds, Evens}.