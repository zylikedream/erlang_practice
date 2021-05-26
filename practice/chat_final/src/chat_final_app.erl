%%%-------------------------------------------------------------------
%% @doc chat_ranch public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_final_app).

-behaviour(application).
-include("data.hrl").
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ets:new(account, [set, public, named_table, {keypos, #account_info.account}]),
    {ok, _} = ranch:start_listener(chatserver, ranch_tcp, #{socket_opts=>[{port, 50001}]}, socket_protocol, []),
    chat_final_sup:start_link(_StartArgs).

stop(_State) ->
    ok.

%% internal functions
