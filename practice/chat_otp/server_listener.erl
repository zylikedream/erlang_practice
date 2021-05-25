-module(server_listener).
-include("data.hrl").
-include("proto.hrl").
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    db:start(),
    ets:new(account, [set, public, named_table, {keypos, #account_info.account}]),
    {ok, Listen} = gen_tcp:listen(50001, [binary, {packet, 2}, {reuseaddr, true}, {active, true}]),
    start_listen(Listen),
    {ok, #{listen=>Listen}}.

start_listen(Listen) ->
    socket_sup:start_socket_handler(Listen).

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    error_logger:info_msg("listener handle call: ~p~n", [_Request]),  
    {reply, ok, State}.

handle_cast({accept, _Socket}, #{listen:=Listen}=State) ->
    error_logger:info_msg("socket accept"),
    start_listen(Listen),
    {noreply, State};
handle_cast(_Msg, State) ->
    error_logger:info_msg("listener handle cast: ~p~n", [_Msg]),  
    {noreply, State}.

handle_info({'EXIT', Pid, _}, State) ->  
  error_logger:info_msg("listener handle exit: ~p~n", [Pid]),  
  {noreply, State};  

handle_info(_Info, State) ->
    error_logger:info_msg("listener handle info: ~p~n", [_Info]),  
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("listener terminate: ~p~n", [_Reason]),  
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
