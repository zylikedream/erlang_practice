-module(area_server1).
-export([start/0, area/2]).

start() -> spawn(fun() -> loop() end).

area(Pid, What) ->
    rpc(Pid, What).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> 
            Response
    end.

loop() ->
    receive
        {From, {rectangle, W, H}} ->
            From ! {self(), W*H},
            loop();
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From, Other} ->
            From ! {self(), {Other, error}}
    end.
