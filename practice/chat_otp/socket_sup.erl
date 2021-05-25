-module(socket_sup).
-behaviour(supervisor).
-include("data.hrl").

%% API
-export([start_link/0, start_socket_handler/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_socket_handler(Listen) ->
    supervisor:start_child(?MODULE, [Listen]).

init([]) ->
    Restart = #{
        strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},
    Wokers = [
        #{
            id => socket_handler,
            start => {socket_handler, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [socket_handler, db, proto, common]
        }
    ],
    {ok, {Restart, Wokers}}.