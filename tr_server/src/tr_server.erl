%%%-------------------------------------------------------------------
%%% @author zy<zhangyi17@iqiyi.com>
%%% @copyright 2021 iqiyi
%%% @doc
%%% a rpc tcp server use otp
%%% @end
%%% @date: 2021-05-27
%%%-------------------------------------------------------------------
-module(tr_server).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
%% API 
-export([start_link/0, start_link/1, get_count/0, stop/0]).
%% gen_server回调函数 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-define(DEFALUT_PORT, 10555).
-record(state, {port, lsock, request_count=0}).
%%%=================================================================== 
%%% API 
%%%=================================================================== 
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?DEFALUT_PORT], []).
start_link(Port) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

get_count()->
    gen_server:call(?SERVER, get_count).

stop() ->
    gen_server:cast(?SERVER, stop).
%%%=================================================================== 
%%% gen_server回调函数 
%%%=================================================================== 
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
    {ok, #state{port=Port, lsock=LSock}, 0}. 

handle_call(_Request, _From, State) -> 
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) -> 
    {stop, normal, State};
handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info({tcp, Socket, RawData}, State) -> 
    do_rpc(Socket, RawData),
    ReqCnt = State#state.request_count,
    {noreply, State#state{request_count = ReqCnt}};
handle_info(timeout, #state{lsock=LSock}=State) -> 
    {ok, _Sock} = gen_tcp:accept(LSock),
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
do_rpc(Socket, RawData) ->
    try 
        {M, F, A} = split_out_mfa(RawData),
        io:format("mfa = ~p~n", [{M, F, A}]),
        Result = apply(M, F, A),
        gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
      _Class:Err ->
        gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [M, F, A]} = re:run(MFA, "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$", [{capture, [1, 2, 3], list}, ungreedy]),
    {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.


%%% 单元测试
start_test() ->
    {ok, _} = start_link(10555).