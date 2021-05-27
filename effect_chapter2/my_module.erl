-module(my_module).
-export([pie/0, print/1, current_pos/0]).

pie() ->
    3.14.

print(Term) ->
    io:format("The Term value is: ~p.~n", [Term]).
current_pos() ->
    io:format("File:~p, line:~p, module:~p ~n", [?FILE, ?LINE, ?MODULE]).