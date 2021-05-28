%%%-------------------------------------------------------------------
%%% @author zy<zhangyi17@iqiyi.com>
%%% @copyright 2021 iqiyi
%%% @doc
%%% 
%%% @end
%%% @create: 2021-05-28
%%%-------------------------------------------------------------------
-module(die_please).
-behaviour(gen_server).
%% API 
-export([start_link/0, go_die/0]).
%% gen_server回调函数 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-define(SLEEP_TIME, 4000).
-record(state, {}).

%%%=================================================================== 
%%% API 
%%%=================================================================== 
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

go_die() ->
    timer:sleep(?SLEEP_TIME),
    go_die = right_now.

%%%=================================================================== 
%%% gen_server回调函数 
%%%=================================================================== 
init([]) ->
    {ok, #state{}, ?SLEEP_TIME}. 

handle_call(_Request, _From, State) -> 
    Reply = ok, 
    {reply, Reply, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(timeout, State) -> 
    go_die = right_now, % 引发异常
    {noreply, State};

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok. 

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}. 

%%%=================================================================== 
%%% 内部函数 
%%%===================================================================
