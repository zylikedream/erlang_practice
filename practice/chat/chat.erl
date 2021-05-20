-module(chat).
-export([start/0]).
-include("data.hrl").
-include("proto.hrl").
-define(GLOBAL_ACCOUNT, "@--global--@").

start() ->
    db:start(),
    start_server(),
    start_friend_service(),
    start_chat_service().

start_service(Service, Loop) ->
    register(Service, Pid=spawn(Loop)),
    spawn(fun() ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, _Pid, Why} ->
                io:format("service ~p down, why:~p~n", [Service, Why]),
                start_service(Service, Loop)
        end
    end).

start_friend_service() ->
    start_service(friend_service, fun friend_loop/0).

start_chat_service() ->
    start_service(chat_service, fun chat_loop/0).

start_server() ->
    ets:new(account, [set, public, named_table, {keypos, #account_info.account}]),
    start_paraller_server().

start_paraller_server() ->
    {ok, Listen} = gen_tcp:listen(50001, [binary, {packet, 2}, {reuseaddr, true}, {active, true}]),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    socket_loop(Socket, "").

socket_loop(Socket, Account) ->
    try
        receive
            {tcp, Socket, Bin} ->
                {{MsgId, Msg}, _} = proto:decode(Bin),
                io:format("recv packet id:~w msg:~w~n", [MsgId, Msg]),
                Acc = handle_account_msg({Socket, Account, MsgId, Msg}),
                socket_loop(Socket, Acc);
            {tcp_closed, Socket}->
                io:format("Account ~s socket ~w close ~n", [Account, Socket]),
                disconnect(get_account(Account));
            {chat_push, Acc, ChatLog}->
                spawn(fun() -> handle_chat_push(Acc, ChatLog) end),
                socket_loop(Socket, Account);
            {service, MsgId, {ack, Ack}, {data, Data}} ->
                async_callback(Socket, MsgId, Ack, Data),
                socket_loop(Socket, Account)
        end
    catch
        Exception:Reason -> 
            io:format("catch exception:~p reason:~p~n", [Exception, Reason]),
            disconnect(get_account(Account)),
            gen_tcp:close(Socket)
    end.

ack(MsgId, Code, Info) ->
    #sc_ack_msg{id=MsgId ,code=Code, info=Info}.

get_account(Account) ->
    case ets:lookup(account, Account) of 
        []-> [];
        [AccInfo] -> AccInfo
    end.


disconnect([]) -> 
    void;
disconnect(AccInfo) -> 
    on_user_offline(AccInfo),
    NewAccInfo = AccInfo#account_info{logout_time=timestamp(), socket=[]},
    ets:delete(account, AccInfo#account_info.account),
    db:update(NewAccInfo).


handle_chat_push(Acc, ChatLog) ->
    AccInfo = get_account(Acc),
    case ChatLog#chat_log.type of  
       private ->
           chat_private_push(AccInfo, ChatLog);
       global ->
           chat_global_push(ChatLog)
    end.

chat_log2chat_msg([]) ->
    [];
chat_log2chat_msg(ChatLog) ->
    #chat_log{src=Src, dst=Dst, content=Content, chat_time=ChatTime, type=Type} = ChatLog,
    #sc_chat_msg{src=Src, dst=Dst, content=Content, chat_time=ChatTime, type=Type}.


chat_private_push([], _ChatLog) ->
    void;
chat_private_push(AccInfo, ChatLog) ->
    gen_tcp:send(AccInfo#account_info.socket, proto:encode(?MSG_CHAT_MSG, chat_log2chat_msg(ChatLog))). 

chat_global_push(ChatLog) ->
    lists:map(fun(AccInfo) ->  chat_private_push(AccInfo, ChatLog) end, ets:tab2list(account)).


handle_account_msg({Socket, Account, MsgId, Msg}) ->
    AccInfo = get_account(Account),
    {Acc, Ack} = case MsgId of
        ?MSG_LOGIN -> do_login(Socket, AccInfo, Msg);
        ?MSG_REGISTER-> {Account, do_register(Socket, Msg)};
        ?MSG_LOGOUT-> {Account, do_logout(Socket, AccInfo, Msg)};
        ?MSG_UNREGISTER-> {Account, do_unregister(Socket, AccInfo, Msg)};

        ?MSG_ADD_FRIEND -> {Account, do_add_friend(Socket, AccInfo, Msg)};
        ?MSG_SEARCH_FRIEND -> {Account, do_search_friend(Socket, AccInfo, Msg)};
        ?MSG_REM_FRIEND -> {Account, do_rem_friend(Socket, AccInfo, Msg)};
        ?MSG_LIST_FRIEND -> {Account, do_list_friend(Socket, AccInfo, Msg)};

        ?MSG_CHAT_ALL -> {Account, do_chat_all(Socket, AccInfo, Msg)};
        ?MSG_CHAT_PRIVATE -> {Account, do_chat_private(Socket, AccInfo, Msg)};
        ?MSG_CHAT_LOG-> {Account, do_chat_log(Socket, AccInfo, Msg)}
    end,
    io:format("handler Msg id:~p data:~p ack:~p~n", [MsgId, Msg, Ack]),
    % ack消息
    case Ack of 
        [] -> void; %异步调用
        _ -> %同步调用
            gen_tcp:send(Socket, proto:encode(?MSG_ACK, Ack))
    end,
    Acc.

friend_simple2friend_info([]) ->
    [];
friend_simple2friend_info(FriendSimple) ->
    #friend_simple{account=Account, friend_time=FriendTime} = FriendSimple,
    #sc_friend_info_msg{account=Account, friend_time=FriendTime}.

wait_sync_msg(MsgId) ->
    receive 
        {service, MsgId, {ack, Ack}, {data, Data}} -> 
            {Ack, Data}
    after 3000 ->
        {ack(MsgId, ?CODE_ERROR_INTERVAL, "timeout"), {}}
    end.

call_service(Service, MsgId, Msg) ->
    case whereis(Service) of 
        undefined ->
           {service_unvalid, Service};
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    Service ! {self(), MsgId, Msg};
                false ->
                    {service_unvalid, Service}
            end
    end.

sync_call(Service, MsgId, Msg) ->
    case call_service(Service, MsgId, Msg) of 
        {service_unvalid, _} ->
            ack(MsgId, ?CODE_ERROR_INTERVAL, "service unvalid");
        _ -> 
            wait_sync_msg(MsgId)
    end.


async_call(Service, MsgId, Msg) ->
    case call_service(Service, MsgId, Msg) of 
        {service_unvalid, _} ->
            ack(MsgId, ?CODE_ERROR_INTERVAL, "service unvalid");
        _ ->
            []
    end.

async_callback(Socket, MsgId, Ack, Data) ->
    case MsgId of 
        ?MSG_CHAT_LOG ->
            case Ack#sc_ack_msg.code of 
                ?CODE_OK ->
                    Logs = Data,
                    io:format("Logs=~p~n", [Logs]),
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

do_chat_all(_Socket, "", _Msg) ->
    ack(?MSG_CHAT_ALL, ?CODE_ERROR_INTERVAL, "need login first");
do_chat_all(_Socket, Account, Msg) ->
    async_call(chat_service, ?MSG_CHAT_ALL, {Account, Msg#cs_chat_all.content}).

do_chat_private(_Socket, [], _Msg) ->
    ack(?MSG_CHAT_PRIVATE, ?CODE_ERROR_INTERVAL, "need login first");
do_chat_private(_Socket, AccInfo, Msg) ->
    async_call(chat_service, ?MSG_CHAT_PRIVATE, {AccInfo, Msg#cs_chat_private.dst, Msg#cs_chat_private.content}).

do_chat_log(_Socket, [], _Msg) ->
    ack(?MSG_CHAT_LOG, ?CODE_ERROR_INTERVAL, "need login first");
do_chat_log(_Socket, AccInfo, _Msg) ->
    async_call(chat_service, ?MSG_CHAT_LOG, {AccInfo}).


do_add_friend(_Socket, [], _Msg) ->
    ack(?MSG_ADD_FRIEND, ?CODE_ERROR_INTERVAL, "need login first");
do_add_friend(_Socket, AccInfo, Msg) ->
    case get_account(Msg#cs_add_friend_msg.account_friend) of 
        [] -> ack(?MSG_ADD_FRIEND, ?CODE_ERROR_INTERVAL, "frd not found");
        FrdAccInfo ->
            {Ack, _} = sync_call(friend_service, ?MSG_ADD_FRIEND, {AccInfo, FrdAccInfo}),
            Ack
    end.

do_search_friend(_Socket, [], _Msg) ->
    ack(?MSG_SEARCH_FRIEND, ?CODE_ERROR_INTERVAL, "need login first");
do_search_friend(Socket, AccInfo, Msg) ->
    {Ack, Friend} = sync_call(friend_service, ?MSG_SEARCH_FRIEND, {AccInfo, Msg#cs_search_friend_msg.account_friend}),
    case Ack#sc_ack_msg.code of 
        ?CODE_OK ->
            FrdMsg = #sc_friend_info_msg{account=Friend#friend_simple.account, friend_time=Friend#friend_simple.friend_time},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_INFO, FrdMsg));
        _ -> void
    end,
    Ack.

do_rem_friend(_Socket, [], _Msg) ->
    ack(?MSG_REM_FRIEND, ?CODE_ERROR_INTERVAL, "need login first");
do_rem_friend(_Socket, AccInfo, Msg) ->
    {Ack, _} = sync_call(friend_service, ?MSG_REM_FRIEND, {AccInfo, Msg#cs_rem_friend_msg.account_friend}),
    Ack.


do_list_friend(_Socket, [], _Msg) ->
    ack(?MSG_LIST_FRIEND, ?CODE_ERROR_INTERVAL, "need login first");
do_list_friend(Socket, AccInfo, _Msg) ->
    {Ack, Friends} = sync_call(friend_service, ?MSG_LIST_FRIEND, {AccInfo}),
    case Ack#sc_ack_msg.code of 
        ?CODE_OK ->
            FriendsMsg = #sc_friend_list_msg{friends=lists:map(fun(Frd) -> friend_simple2friend_info(Frd) end, Friends)},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_LIST, FriendsMsg));
        _ -> void
    end,
    Ack.

do_login(Socket, [], Msg) ->
    #cs_login_msg{account=Acc, passwd=Pass} = Msg,
    case db:find_account_info(Acc) of
        [] -> {"", ack(?MSG_LOGIN, ?CODE_ERROR_INTERVAL, "account not found. need register first")};
        [AccInfo] -> 
            if 
                AccInfo#account_info.passwd =/= Pass -> {"", ack(?MSG_LOGIN, ?CODE_ERROR_INTERVAL, "passwd or account invalid")};
                true ->
                    io:format("account_info:~p~n", [AccInfo]),
                    U = AccInfo#account_info{login_time=timestamp(), socket=Socket},
                    ets:insert(account, U),
                    db:update(U),
                    on_user_online(AccInfo),
                    {Acc, ack(?MSG_LOGIN, ?CODE_OK, "login success")}
            end
    end;
do_login(_, _, _) ->
    {"", ack(?MSG_LOGIN, ?CODE_OK, "already logined")}.


do_logout(_Socket, [], __Msg) ->
    ack(?MSG_LOGOUT, ?CODE_ERROR_INTERVAL, "logout failed. need login first");
do_logout(_Socket, _, _Msg) ->
    %登出不处理，再断开连接时处理
    ack(?MSG_LOGOUT, ?CODE_OK, "logout success").

do_unregister(_Socket, [], _Msg) ->
    ack(?MSG_UNREGISTER, ?CODE_ERROR_INTERVAL, "unregister failed. need login first");
do_unregister(_Socket, AccInfo, _Msg) ->
    ets:insert(account, AccInfo#account_info{delete_time=timestamp()}),
    db:update(AccInfo#account_info{delete_time=timestamp()}),
    ack(?MSG_UNREGISTER, ?CODE_OK, "unregister success").

do_register(_Socket, Msg) ->
    #cs_register_msg{account=Acc, passwd=Pass} = Msg,
    case db:find_account_info(Acc) of
        [] -> 
            db:insert(#account_info{account=Acc, passwd=Pass, register_time=timestamp(), user_info=#user_info{account=Acc}}),
            ack(?MSG_REGISTER, ?CODE_OK, "registersuccess");
        _ -> ack(?MSG_REGISTER, ?CODE_ERROR_INTERVAL, "already reistered")
    end.

timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

on_user_online(_AccInfo) ->
    void.
on_user_offline(_AccInfo) ->
    void.

pack_service_result(MsgId, Ack, Data) ->
    {service, MsgId, {ack, Ack}, {data, Data}}.

friend_loop() ->
    receive
        {From, ?MSG_ADD_FRIEND, Msg} -> 
            From ! pack_service_result(?MSG_ADD_FRIEND, handle_add_friend(Msg), {});
        {From, ?MSG_SEARCH_FRIEND, Msg} -> 
            {Friend, Ack} = handle_search_friend(Msg),
            From ! pack_service_result(?MSG_SEARCH_FRIEND, Ack, Friend);
        {From, ?MSG_REM_FRIEND, Msg} -> 
            From ! pack_service_result(?MSG_REM_FRIEND, handle_rem_friend(Msg), {});
        {From, ?MSG_LIST_FRIEND, Msg} -> 
            {FriendList, Ack} = handle_list_friend(Msg),
            From ! pack_service_result(?MSG_LIST_FRIEND, Ack, FriendList)
    end,
    friend_loop().

find_friend([], _) -> [];
find_friend([F|_], Acc) when F#friend_simple.account =:= Acc -> F;
find_friend([_|Friends], Acc) -> find_friend(Friends, Acc).

default_friend(Acc) ->
    Friends = #friend_info{account=Acc, friends=[]},
    db:insert(Friends),
    Friends.

handle_add_friend({SelfAccInfo, FrdAccInfo}) when SelfAccInfo#account_info.account =:= FrdAccInfo#account_info.account ->
    ack(?MSG_ADD_FRIEND, ?CODE_ERROR_INTERVAL, "Can't add self");
handle_add_friend({SelfAccInfo, FrdAccInfo})->
    Ack = inner_handle_add_friend({SelfAccInfo, FrdAccInfo}),
    if 
        Ack#sc_ack_msg.code =/= ?CODE_OK -> Ack;
        true -> inner_handle_add_friend({FrdAccInfo, SelfAccInfo})
    end.

inner_handle_add_friend({SelfAccInfo, FrdAccInfo})->
    AccSelf = SelfAccInfo#account_info.account,
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    AccFrd = FrdAccInfo#account_info.account,
    case find_friend(Friends#friend_info.friends, AccFrd) of
        [] -> 
            NewFriends = [#friend_simple{account=AccFrd, friend_time=timestamp()}|Friends#friend_info.friends],
            db:update(Friends#friend_info{friends=NewFriends}),
            ack(?MSG_ADD_FRIEND, ?CODE_OK, "add friend success");
        _ -> ack(?MSG_ADD_FRIEND, ?CODE_ERROR_INTERVAL, "already friend")
    end.


handle_search_friend({SelfAccInfo, AccFrd})->
    AccSelf = SelfAccInfo#account_info.account,
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case find_friend(Friends#friend_info.friends, AccFrd) of
        [] -> {#friend_simple{}, ack(?MSG_SEARCH_FRIEND, ?CODE_ERROR_INTERVAL, "not find")};
        Friend -> {Friend, ack(?MSG_SEARCH_FRIEND, ?CODE_OK, "find success")}
    end.


handle_rem_friend({SelfAccInfo, AccFrd})->
    AccSelf = SelfAccInfo#account_info.account,
    Ack = inner_handle_rem_friend({AccSelf, AccFrd}),
    if 
        Ack#sc_ack_msg.code =/= ?CODE_OK -> Ack;
        true -> inner_handle_rem_friend({AccFrd, AccSelf})
    end.

inner_handle_rem_friend({AccSelf, AccFrd})->
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case find_friend(Friends#friend_info.friends, AccFrd) of
        [] -> ack(?MSG_REM_FRIEND, ?CODE_OK, "not friend no need remove");
        Friend -> 
            NewFriends = Friends#friend_info.friends--[Friend],
            db:update(Friends#friend_info{friends=NewFriends}),
            ack(?MSG_REM_FRIEND, ?CODE_OK, "success")
    end.

handle_list_friend({SelfAccInfo})->
    AccSelf = SelfAccInfo#account_info.account,
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    {Friends#friend_info.friends, ack(?MSG_LIST_FRIEND, ?CODE_OK, "success")}.

chat_loop()->
    receive
        {From, ?MSG_CHAT_PRIVATE, Msg} ->
            From ! pack_service_result(?MSG_CHAT_PRIVATE, chat_private(From, Msg), {});
        {From, ?MSG_CHAT_ALL, Msg} ->
            From ! pack_service_result(?MSG_CHAT_ALL, chat_global(From, Msg), {});
        {From, ?MSG_CHAT_LOG, Msg} ->
            {Logs, Ack} = chat_log(Msg),
            From ! pack_service_result(?MSG_CHAT_LOG, Ack, Logs)
    end,
    chat_loop().

chat_push([], _Acc, _ChatLog) ->
    void;
chat_push(Server, Acc, ChatLog) ->
    Server ! {chat_push, Acc, ChatLog}.

default_chat(Src) ->
    ChatInfo = #chat_info{account=Src},
    db:insert(ChatInfo),
    ChatInfo.

add_chat_log(Acc, ChatLog) ->
    ChatInfo = case db:find_chat_info(Acc) of 
        [] -> default_chat(Acc);
        [C] -> C
    end,
    NewChatLog = [ChatLog|ChatInfo#chat_info.logs],
    db:update(ChatInfo#chat_info{logs=NewChatLog}).

chat_private(_From, {SrcAccInfo, DstAcc, _Content}) when SrcAccInfo#account_info.account =:= DstAcc ->
    ack(?MSG_CHAT_PRIVATE, ?CODE_ERROR_INTERVAL, "can't chat to self");
chat_private(From, {SrcAccInfo, DstAcc, Content}) ->
    SrcAcc= SrcAccInfo#account_info.account,
    ChatLog = #chat_log{src=SrcAcc, dst=DstAcc, chat_time=timestamp(), content=Content, type=private},
    add_chat_log(SrcAcc, ChatLog),
    add_chat_log(DstAcc, ChatLog),
    chat_push(From, SrcAcc, ChatLog),
    chat_push(From, DstAcc, ChatLog),
    ack(?MSG_CHAT_PRIVATE, ?CODE_OK, "chat private success").

chat_global(From, {SrcAccInfo, Content}) ->
    SrcAcc = SrcAccInfo#account_info.account,
    ChatLog = #chat_log{src=?GLOBAL_ACCOUNT, dst=SrcAcc, chat_time=timestamp(), content=Content, type=global},
    %全局聊天单独保存%
    add_chat_log(?GLOBAL_ACCOUNT, ChatLog),
    chat_push(From, SrcAcc, ChatLog),
    ack(?MSG_CHAT_ALL, ?CODE_OK, "chat global success").


chat_log({AccInfo}) ->
    throw("chat log error"),
    Acc = AccInfo#account_info.account,
    ChatInfos = db:find_one(chat_info, fun(ChatInfo) -> 
        ChatAcc = ChatInfo#chat_info.account,
        ChatAcc =:= Acc orelse ChatAcc =:= ?GLOBAL_ACCOUNT
    end), 
    {lists:foldl(fun(ChatInfo, Logs) -> Logs ++ ChatInfo#chat_info.logs end, [], ChatInfos), ack(?MSG_CHAT_LOG, ?CODE_OK, "get chat log success")}.