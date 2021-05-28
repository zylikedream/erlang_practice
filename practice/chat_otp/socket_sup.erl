-module(socket_sup).
-behaviour(supervisor).
-include("data.hrl").

%% API
-export([start_link/0, start_socket_handler/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_socket_handler(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).

init([]) ->
    Restart = #{
        strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 0,
        period => 1},
    Wokers = [
        #{
            id => socket_handler,
            start => {socket_handler, start_link, []},
            restart => temporary, % permanent | transient | temporary
            shutdown => brutal_kill,
            type => worker, % worker | supervisor
            modules => [socket_handler, db, proto, common]
        }
    ],
    {ok, {Restart, Wokers}}.