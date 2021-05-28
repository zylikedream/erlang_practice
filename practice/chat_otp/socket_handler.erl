-module(socket_handler).
-behaviour(gen_server).
-include("proto.hrl").
-include("data.hrl").

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    {ok, #{socket=>Socket, account=>""}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    error_logger:info_msg("cast Msg:~p~n", [_Msg]),
    {noreply, State}.

handle_info({tcp_closed, Socket}, #{account:=Account}=State) ->
    error_logger:info_msg("Account ~s socket ~w close ~n", [Account, Socket]),
    disconnect(get_account(Account)),
    {noreply, State#{socket:=[], account:=""}};
handle_info({tcp, Socket, Bin}, State) ->
    {{MsgId, Msg}, _} = proto:decode(Bin),
    error_logger:info_msg("recv packet id:~w msg:~w~n", [MsgId, Msg]),
    NewState = handle_msg(State, Socket, MsgId, Msg),
    {noreply, NewState};
handle_info({async, Msg}, #{socket := Socket}=State) ->
    async_callback(Socket, Msg),
    {noreply, State};
handle_info({chat_push, {Account, ChatLog}}, State) ->
    spawn(fun() -> handle_chat_push(Account, ChatLog) end),
    {noreply, State};
handle_info(_Info, State) ->
    error_logger:info_msg("info Msg:~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_account(Account) ->
    case ets:lookup(account, Account) of 
        []-> [];
        [AccInfo] -> AccInfo
    end.

handle_msg(#{account:=Account}=State, Socket, MsgId, Msg) ->
    AccInfo = get_account(Account),
    {NewState, Ack} = case MsgId of
        ?MSG_LOGIN -> do_login(State, Socket, AccInfo, Msg);
        ?MSG_REGISTER-> {State, do_register(Msg)};
        ?MSG_LOGOUT-> {State, do_logout(AccInfo, Msg)};
        ?MSG_UNREGISTER-> {State, do_unregister(AccInfo, Msg)};

        ?MSG_ADD_FRIEND -> {State, do_add_friend(AccInfo, Msg)};
        ?MSG_SEARCH_FRIEND -> {State, do_search_friend(Socket, AccInfo, Msg)};
        ?MSG_REM_FRIEND -> {State, do_rem_friend(AccInfo, Msg)};
        ?MSG_LIST_FRIEND -> {State, do_list_friend(Socket, AccInfo, Msg)};

        ?MSG_CHAT_ALL -> {State, do_chat_all(AccInfo, Msg)};
        ?MSG_CHAT_PRIVATE -> {State, do_chat_private(AccInfo, Msg)};
        ?MSG_CHAT_LOG-> {State, do_chat_log(AccInfo, Msg)}
    end,
    error_logger:info_msg("handler Msg id:~p data:~p ack:~p~n", [MsgId, Msg, Ack]),
    % ack消息
    case Ack of 
        [] -> void; %异步调用
        _ -> %同步调用
            gen_tcp:send(Socket, proto:encode(?MSG_ACK, Ack))
    end,
    NewState.

do_login(State, Socket, [], Msg) ->
    #cs_login_msg{account=Acc, passwd=Pass} = Msg,
    case db:find_account_info(Acc) of
        [] -> {State, common:ack(?MSG_LOGIN, ?CODE_ERROR_INTERVAL, "account not found. need register first")};
        [AccInfo] -> 
            if 
                AccInfo#account_info.passwd =/= Pass -> {State, common:ack(?MSG_LOGIN, ?CODE_ERROR_INTERVAL, "passwd or account invalid")};
                true ->
                    error_logger:info_msg("account_info:~p~n", [AccInfo]),
                    U = AccInfo#account_info{login_time=common:timestamp(), socket=Socket},
                    ets:insert(account, U),
                    db:update(U),
                    on_user_online(AccInfo),
                    {State#{account:=Acc}, common:ack(?MSG_LOGIN, ?CODE_OK, "login success")}
            end
    end;
do_login(State, _, _, _) ->
    {State, common:ack(?MSG_LOGIN, ?CODE_OK, "already logined")}.


do_logout([], __Msg) ->
    common:ack(?MSG_LOGOUT, ?CODE_ERROR_INTERVAL, "logout failed. need login first");
do_logout(_, _Msg) ->
    %登出不处理，再断开连接时处理
    common:ack(?MSG_LOGOUT, ?CODE_OK, "logout success").

-spec do_unregister([] | #account_info{}, _) -> any().
do_unregister([], _Msg) ->
    common:ack(?MSG_UNREGISTER, ?CODE_ERROR_INTERVAL, "unregister failed. need login first");
do_unregister(AccInfo, _Msg) ->
    ets:insert(account, AccInfo#account_info{delete_time=common:timestamp()}),
    db:update(AccInfo#account_info{delete_time=common:timestamp()}),
    common:ack(?MSG_UNREGISTER, ?CODE_OK, "unregister success").

do_register(Msg) ->
    #cs_register_msg{account=Acc, passwd=Pass} = Msg,
    case db:find_account_info(Acc) of
        [] -> 
            db:insert(#account_info{account=Acc, passwd=Pass, register_time=common:timestamp(), user_info=#user_info{account=Acc}}),
            common:ack(?MSG_REGISTER, ?CODE_OK, "registersuccess");
        _ -> common:ack(?MSG_REGISTER, ?CODE_ERROR_INTERVAL, "already reistered")
    end.



on_user_online(_AccInfo) ->
    void.
on_user_offline(_AccInfo) ->
    void.


disconnect([]) -> 
    void;
disconnect(AccInfo) -> 
    on_user_offline(AccInfo),
    NewAccInfo = AccInfo#account_info{logout_time=common:timestamp(), socket=[]},
    ets:delete(account, AccInfo#account_info.account),
    db:update(NewAccInfo).

handle_chat_push(Account, ChatLog) ->
    case ChatLog#chat_log.type of  
       private ->
           AccInfo = get_account(Account),
           chat_private_push(AccInfo#account_info.socket, ChatLog);
       global ->
           chat_global_push(ChatLog)
    end.

chat_log2chat_msg([]) ->
    [];
chat_log2chat_msg(ChatLog) ->
    #chat_log{src=Src, dst=Dst, content=Content, chat_time=ChatTime, type=Type} = ChatLog,
    #sc_chat_msg{src=Src, dst=Dst, content=Content, chat_time=ChatTime, type=Type}.


chat_private_push(Socket, ChatLog) ->
    gen_tcp:send(Socket, proto:encode(?MSG_CHAT_MSG, chat_log2chat_msg(ChatLog))). 
chat_global_push(ChatLog) ->
    lists:map(fun(AccInfo) ->  chat_private_push(AccInfo#account_info.socket, ChatLog) end, ets:tab2list(account)).


friend_simple2friend_info([]) ->
    [];
friend_simple2friend_info(FriendSimple) ->
    #friend_simple{account=Account, friend_time=FriendTime} = FriendSimple,
    #sc_friend_info_msg{account=Account, friend_time=FriendTime}.

sync_call(Service, MsgId, Msg) ->
    Reply = gen_server:call(Service, {MsgId, Msg}, 3000),
    {MsgId, Ack, Data} = common:unpack_service_result(Reply),
    {Ack, Data}.


async_call(Service, MsgId, Msg) ->
    gen_server:cast(Service, {MsgId, Msg, self()}),
    [].

async_callback(Socket, Msg) ->
    {MsgId, Ack, Data} = common:unpack_service_result(Msg),
    case MsgId of 
        ?MSG_CHAT_LOG ->
            case Ack#sc_ack_msg.code of 
                ?CODE_OK ->
                    Logs = Data,
                    error_logger:info_msg("Logs=~p~n", [Logs]),
                    LogsMsg = #sc_chat_log{logs=lists:map(fun(Log) -> chat_log2chat_msg(Log) end, Logs)},
                    gen_tcp:send(Socket, proto:encode(?MSG_CHAT_LOG_INFO, LogsMsg));
                _ -> void
            end;
        ?MSG_CHAT_PRIVATE->
            void;
        ?MSG_CHAT_ALL->
            void
    end,
    gen_tcp:send(Socket, proto:encode(?MSG_ACK, Ack)).

do_chat_all("", _Msg) ->
    common:ack(?MSG_CHAT_ALL, ?CODE_ERROR_INTERVAL, "need login first");
do_chat_all(AccInfo, Msg) ->
    async_call(service_chat, ?MSG_CHAT_ALL, {AccInfo, Msg#cs_chat_all.content}).

do_chat_private([], _Msg) ->
    common:ack(?MSG_CHAT_PRIVATE, ?CODE_ERROR_INTERVAL, "need login first");
do_chat_private(AccInfo, Msg) ->
    async_call(service_chat, ?MSG_CHAT_PRIVATE, {AccInfo, Msg#cs_chat_private.dst, Msg#cs_chat_private.content}).

do_chat_log([], _Msg) ->
    common:ack(?MSG_CHAT_LOG, ?CODE_ERROR_INTERVAL, "need login first");
do_chat_log(AccInfo, _Msg) ->
    async_call(service_chat, ?MSG_CHAT_LOG, {AccInfo}).

do_add_friend([], _Msg) ->
    common:ack(?MSG_ADD_FRIEND, ?CODE_ERROR_INTERVAL, "need login first");
do_add_friend(AccInfo, Msg) ->
    case get_account(Msg#cs_add_friend_msg.account_friend) of 
        [] -> common:ack(?MSG_ADD_FRIEND, ?CODE_ERROR_INTERVAL, "frd not found");
        FrdAccInfo ->
            {Ack, _} = sync_call(service_friend, ?MSG_ADD_FRIEND, {AccInfo, FrdAccInfo}),
            Ack
    end.

do_search_friend(_Socket, [], _Msg) ->
    common:ack(?MSG_SEARCH_FRIEND, ?CODE_ERROR_INTERVAL, "need login first");
do_search_friend(Socket, AccInfo, Msg) ->
    {Ack, Friend} = sync_call(service_friend, ?MSG_SEARCH_FRIEND, {AccInfo, Msg#cs_search_friend_msg.account_friend}),
    case Ack#sc_ack_msg.code of 
        ?CODE_OK ->
            FrdMsg = #sc_friend_info_msg{account=Friend#friend_simple.account, friend_time=Friend#friend_simple.friend_time},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_INFO, FrdMsg));
        _ -> void
    end,
    Ack.

do_rem_friend([], _Msg) ->
    common:ack(?MSG_REM_FRIEND, ?CODE_ERROR_INTERVAL, "need login first");
do_rem_friend(AccInfo, Msg) ->
    {Ack, _} = sync_call(service_friend, ?MSG_REM_FRIEND, {AccInfo, Msg#cs_rem_friend_msg.account_friend}),
    Ack.


do_list_friend(_Socket, [], _Msg) ->
    common:ack(?MSG_LIST_FRIEND, ?CODE_ERROR_INTERVAL, "need login first");
do_list_friend(Socket, AccInfo, _Msg) ->
    {Ack, Friends} = sync_call(service_friend, ?MSG_LIST_FRIEND, {AccInfo}),
    case Ack#sc_ack_msg.code of 
        ?CODE_OK ->
            FriendsMsg = #sc_friend_list_msg{friends=lists:map(fun(Frd) -> friend_simple2friend_info(Frd) end, Friends)},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_LIST, FriendsMsg));
        _ -> void
    end,
    Ack.