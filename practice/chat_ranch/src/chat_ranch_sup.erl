%%%-------------------------------------------------------------------
%% @doc chatserver top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_ranch_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(_Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Restart = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    Wokers = [
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



