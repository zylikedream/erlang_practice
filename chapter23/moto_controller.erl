-module(moto_controller).
-export([add_event_handler/1]).

add_event_handler(Name) ->
    event_handler:add_handler(Name, fun controller/1).

controller(too_hot) ->
    io:format("Turn off the moter~n");
controller(X) ->
    io:format("~w ignored event: ~p~n", [?MODULE, X]).