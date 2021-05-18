-module(test).
-export([test/0]).
test() ->
    X = 1,
    case X of
        1 -> Y = 2;
        _ -> Y = 3
    end,
    io:format("Y=~p~n", [Y]).
