-module(chatserver_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    chatserver_sup:start_link(StartArgs).
stop(_State) ->
    void.