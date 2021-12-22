%%%-------------------------------------------------------------------
%% @doc simple_cache public API
%% @end
%%%-------------------------------------------------------------------

-module(sc_app).
-include_lib("eunit/include/eunit.hrl").
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sc_store:init(),
    sc_sup:start_link().

stop(_State) ->
    ok.

start_test() ->
    start([], []).
%% internal functions
