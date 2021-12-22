%%%-------------------------------------------------------------------
%%% @author zy<zhangyi17@iqiyi.com>
%%% @copyright 2021 iqiyi
%%% @doc
%%% 
%%% @end
%%% @create: 2021-05-28
%%%-------------------------------------------------------------------
-module(custom_error_logger).
-behaviour(gen_event).
%% API 
-export([register_with_logger/0]).
%% gen_server回调函数 
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

%%%=================================================================== 
%%% API 
%%%=================================================================== 
register_with_logger() ->
    error_logger:add_report_handler(?MODULE).

%%%=================================================================== 
%%% gen_server回调函数 
%%%=================================================================== 
init([]) ->
    {ok, #state{}}. 

handle_call(_Request, State) -> 
    Reply = ok, 
    {ok, Reply, State}.

handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
    io:fwrite("ERROR <~p> ~s", [Pid, io_lib:format(Format, Data)]),
    {ok, State};
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
    io:fwrite("WARNING <~p> ~s", [Pid, io_lib:format(Format, Data)]),
    {ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
    io:fwrite("INFO <~p> ~s", [Pid, io_lib:format(Format, Data)]),
    {ok, State};
handle_event(_Event, State) -> 
    {ok, State}.

handle_info(_Info, State) -> 
    {ok, State}.

terminate(_Reason, _State) -> 
    ok. 

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}. 

%%%=================================================================== 
%%% 内部函数 
%%%===================================================================
