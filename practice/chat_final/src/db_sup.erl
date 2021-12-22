%%%-------------------------------------------------------------------
%%% @author zy<zhangyi17@iqiyi.com>
%%% @copyright 2021 iqiyi
%%% @doc
%%% 
%%% @end
%%% @create: 2021-05-28
%%%-------------------------------------------------------------------
-module(db_sup).
-include("db.hrl").
-behaviour(supervisor).
%% API 
-export([start_link/0]).
%% supervisor回调函数 
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() -> 
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, Conn} = mongo_api:connect(single, "127.0.0.1:27017", [{name, dbpool}, {pool_size, 10}], [{database, <<"chat">>}]),
    Server = #{id=>db, start=>{db, start_link, [Conn]},
                restart=>permanent, shutdonw=>2000, type=>worker, modules=>[db]},
    Children = [Server],
    RestartStrategy = #{strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10, period => 60},
    {ok, {RestartStrategy, Children}}.
