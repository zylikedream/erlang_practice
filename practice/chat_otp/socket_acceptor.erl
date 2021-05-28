%%%-------------------------------------------------------------------
%%% @author zy<zhangyi17@iqiyi.com>
%%% @copyright 2021 iqiyi
%%% @doc
%%% 
%%% @end
%%% @create: 2021-05-28
%%%-------------------------------------------------------------------
-module(socket_acceptor).
%% API 
-export([start_link/1]).

start_link(Listen) -> 
    {ok, spawn_link(fun() -> init(Listen) end)}.

%%%=================================================================== 
%%% gen_server回调函数 
%%%=================================================================== 
init(Listen) ->
    loop(Listen).
loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    {ok, SockHandler} = socket_sup:start_socket_handler(Socket),
    ok = gen_tcp:controlling_process(Socket, SockHandler),
    loop(Listen).

