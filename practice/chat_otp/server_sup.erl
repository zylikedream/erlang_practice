-module(server_sup).
-include("data.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

start_link(_Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    db:start(),
    ets:new(account, [set, public, named_table, {keypos, #account_info.account}]),
    Restart = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    Wokers = [
        #{
            id => socket_sup,
            start => {socket_sup, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => supervisor, % worker | supervisor
            modules => [socket_sup]
        },
        #{
            id => {socket_acceptor_sup,1 },
            start => {socket_acceptor_sup, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [socket_acceptor_sup]
        }, 
        #{
            id => service_chat,
            start => {service_chat, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [service_chat, common, db]
        },
        #{
            id => service_friend,
            start => {service_friend, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [service_friend, common, db]
        }
    ],
    {ok, {Restart, Wokers}}.


