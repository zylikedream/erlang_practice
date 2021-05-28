%%%-------------------------------------------------------------------
%%% @author zy<zhangyi17@iqiyi.com>
%%% @copyright 2021 iqiyi
%%% @doc
%%% 
%%% @end
%%% @create: 2021-05-28
%%%-------------------------------------------------------------------
-module(socket_acceptor_sup).
-behaviour(supervisor).
%% API 
-export([start_link/0]).
%% supervisor回调函数 
-export([init/1]).
-define(SERVER, ?MODULE).

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() -> 
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, Listen} = gen_tcp:listen(50001, [binary, {packet, 2}, {reuseaddr, true}, {active, true}]),
    Servers = [#{id=>{socket_acceptor, Id}, start=>{socket_acceptor, start_link, [Listen]},
              restart=>permanent, shutdown=>2000, type=>worker, modules=>[worker_name]} || Id <- lists:seq(1, 10)],
    Children = Servers,
    RestartStrategy = #{strategy => one_for_all, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
                       intensity => 10,period => 60},
    {ok, {RestartStrategy, Children}}.
