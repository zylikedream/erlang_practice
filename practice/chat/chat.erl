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

start_friend_service() ->
    register(friend_service, spawn(fun() -> friend_loop() end)).

start_chat_service() ->
    register(chat_service, spawn(fun() -> chat_loop() end)).


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
            socket_loop(Socket, Account)
    end.

ack(Code, Info) ->
    #ack{code=Code, info=Info}.

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
    AckMsg =#sc_ack_msg{id=MsgId, code=Ack#ack.code, info=Ack#ack.info},
    gen_tcp:send(Socket, proto:encode(?MSG_ACK, AckMsg)),
    Acc.

friend_simple2friend_info([]) ->
    [];
friend_simple2friend_info(FriendSimple) ->
    #friend_simple{account=Account, friend_time=FriendTime} = FriendSimple,
    #sc_friend_info_msg{account=Account, friend_time=FriendTime}.

handle_service_msg(Socket, AccInfo) ->
    receive 
        {ack, Ack} -> 
             Ack;
        {?MSG_CHAT_LOG, Logs} ->
            io:format("Logs=~p~n", [Logs]),
            LogsMsg = #sc_chat_log{logs=lists:map(fun(Log) -> chat_log2chat_msg(Log) end, Logs)},
            gen_tcp:send(Socket, proto:encode(?MSG_CHAT_LOG_INFO, LogsMsg)),
            handle_service_msg(Socket, AccInfo);
        {?MSG_SEARCH_FRIEND,  Friend} -> 
            FrdMsg = #sc_friend_info_msg{account=Friend#friend_simple.account, friend_time=Friend#friend_simple.friend_time},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_INFO, FrdMsg)),
            handle_service_msg(Socket, AccInfo);
        {?MSG_LIST_FRIEND, Friends} -> 
            FriendsMsg = #sc_friend_list_msg{friends=lists:map(fun(Frd) -> friend_simple2friend_info(Frd) end, Friends)},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_LIST, FriendsMsg)),
            handle_service_msg(Socket, AccInfo)
    after 3000 ->
        ack(?CODE_ERROR_INTERVAL, "timeout")
    end.

do_chat_all(_Socket, "", _Msg) ->
    ack(?CODE_ERROR_INTERVAL, "need login first");
do_chat_all(Socket, Account, Msg) ->
    chat_service ! {self(), ?MSG_CHAT_ALL, {Account, Msg#cs_chat_all.content}},
    handle_service_msg(Socket, Account).

do_chat_private(_Socket, [], _Msg) ->
    ack(?CODE_ERROR_INTERVAL, "need login first");
do_chat_private(Socket, AccInfo, Msg) ->
    chat_service ! {self(), ?MSG_CHAT_PRIVATE, {AccInfo, Msg#cs_chat_private.dst, Msg#cs_chat_private.content}},
    handle_service_msg(Socket, AccInfo).

do_chat_log(_Socket, [], _Msg) ->
    ack(?CODE_ERROR_INTERVAL, "need login first");
do_chat_log(Socket, AccInfo, _Msg) ->
    chat_service ! {self(), ?MSG_CHAT_LOG, {AccInfo}},
    handle_service_msg(Socket, AccInfo).


do_add_friend(_Socket, [], _Msg) ->
    ack(?CODE_ERROR_INTERVAL, "need login first");
do_add_friend(Socket, AccInfo, Msg) ->
    case get_account(Msg#cs_add_friend_msg.account_friend) of 
        [] -> {AccInfo#account_info.account, ack(?CODE_ERROR_INTERVAL, "frd not found")};
        FrdAccInfo ->
            friend_service ! {self(), ?MSG_ADD_FRIEND, {AccInfo, FrdAccInfo}},
            handle_service_msg(Socket, AccInfo)
        end.

do_search_friend(_Socket, [], _Msg) ->
    ack(?CODE_ERROR_INTERVAL, "need login first");
do_search_friend(Socket, AccInfo, Msg) ->
    friend_service ! {self(), ?MSG_SEARCH_FRIEND, {AccInfo, Msg#cs_search_friend_msg.account_friend}},
    handle_service_msg(Socket, AccInfo).

do_rem_friend(_Socket, [], _Msg) ->
    ack(?CODE_ERROR_INTERVAL, "need login first");
do_rem_friend(Socket, AccInfo, Msg) ->
    friend_service ! {self(), ?MSG_REM_FRIEND, {AccInfo, Msg#cs_rem_friend_msg.account_friend}},
    handle_service_msg(Socket, AccInfo).


do_list_friend(_Socket, [], _Msg) ->
    ack(?CODE_ERROR_INTERVAL, "need login first");
do_list_friend(Socket, AccInfo, _Msg) ->
    friend_service ! {self(), ?MSG_LIST_FRIEND, {AccInfo}},
    handle_service_msg(Socket, AccInfo).

do_login(Socket, [], Msg) ->
    #cs_login_msg{account=Acc, passwd=Pass} = Msg,
    case db:find_account_info(Acc) of
        [] -> {"", ack(?CODE_ERROR_INTERVAL, "account not found. need register first")};
        [AccInfo] -> 
            if 
                AccInfo#account_info.passwd =/= Pass -> {"", ack(?CODE_ERROR_INTERVAL, "passwd or account invalid")};
                true ->
                    io:format("account_info:~p~n", [AccInfo]),
                    U = AccInfo#account_info{login_time=timestamp(), socket=Socket},
                    ets:insert(account, U),
                    db:update(U),
                    on_user_online(AccInfo),
                    {Acc, ack(?CODE_OK, "login success")}
            end
    end;
do_login(_, _, _) ->
    {"", ack(?CODE_OK, "already logined")}.


do_logout(_Socket, [], __Msg) ->
    ack(?CODE_ERROR_INTERVAL, "logout failed. need login first");
do_logout(_Socket, _, _Msg) ->
    %登出不处理，再断开连接时处理
    ack(?CODE_OK, "logout success").

do_unregister(_Socket, [], _Msg) ->
    ack(?CODE_ERROR_INTERVAL, "unregister failed. need login first");
do_unregister(_Socket, AccInfo, _Msg) ->
    ets:insert(account, AccInfo#account_info{delete_time=timestamp()}),
    db:update(AccInfo#account_info{delete_time=timestamp()}),
    ack(?CODE_OK, "unregister success").

do_register(_Socket, Msg) ->
    #cs_register_msg{account=Acc, passwd=Pass} = Msg,
    case db:find_account_info(Acc) of
        [] -> 
            db:insert(#account_info{account=Acc, passwd=Pass, register_time=timestamp(), user_info=#user_info{account=Acc}}),
            ack(?CODE_OK, "registersuccess");
        _ -> ack(?CODE_ERROR_INTERVAL, "already reistered")
    end.

timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

on_user_online(_AccInfo) ->
    void.
on_user_offline(_AccInfo) ->
    void.

friend_loop() ->
    receive
        {From, ?MSG_ADD_FRIEND, Msg} -> 
            From ! {ack, handle_add_friend(Msg)};
        {From, ?MSG_SEARCH_FRIEND, Msg} -> 
            {Friend, AckInfo} = handle_search_friend(Msg),
            From ! {?MSG_SEARCH_FRIEND, Friend},
            From ! {ack, AckInfo};
        {From, ?MSG_REM_FRIEND, Msg} -> 
            From ! {ack, handle_rem_friend(Msg)};
        {From, ?MSG_LIST_FRIEND, Msg} -> 
            {FriendList, AckInfo} = handle_list_friend(Msg),
            From ! {?MSG_LIST_FRIEND, FriendList},
            From ! {ack, AckInfo}
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
    ack(?CODE_ERROR_INTERVAL, "Can't add self");
handle_add_friend({SelfAccInfo, FrdAccInfo})->
    Ack = inner_handle_add_friend({SelfAccInfo, FrdAccInfo}),
    if 
        Ack#ack.code =/= ?CODE_OK -> Ack;
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
            ack(?CODE_OK, "add friend success");
        _ -> ack(?CODE_ERROR_INTERVAL, "already friend")
    end.


handle_search_friend({SelfAccInfo, AccFrd})->
    AccSelf = SelfAccInfo#account_info.account,
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case find_friend(Friends#friend_info.friends, AccFrd) of
        [] -> {#friend_simple{}, ack(?CODE_ERROR_INTERVAL, "not find")};
        Friend -> {Friend, ack(?CODE_OK, "find success")}
    end.


handle_rem_friend({SelfAccInfo, AccFrd})->
    AccSelf = SelfAccInfo#account_info.account,
    Ack = inner_handle_rem_friend({AccSelf, AccFrd}),
    if 
        Ack#ack.code =/= ?CODE_OK -> Ack;
        true -> inner_handle_rem_friend({AccFrd, AccSelf})
    end.

inner_handle_rem_friend({AccSelf, AccFrd})->
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> ack(?CODE_OK, "find friend info failed");
        [F] -> F
    end,
    case find_friend(Friends#friend_info.friends, AccFrd) of
        [] -> ack(?CODE_OK, "not friend no need remove");
        Friend -> 
            NewFriends = Friends#friend_info.friends--[Friend],
            db:update(Friends#friend_info{friends=NewFriends}),
            ack(?CODE_OK, "success")
    end.

handle_list_friend({SelfAccInfo})->
    AccSelf = SelfAccInfo#account_info.account,
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    {Friends#friend_info.friends, ack(?CODE_OK, "success")}.

chat_loop()->
    receive
        {From, ?MSG_CHAT_PRIVATE, Msg} ->
            From ! {ack, chat_private(From, Msg)};
        {From, ?MSG_CHAT_ALL, Msg} ->
            From ! {ack, chat_global(From, Msg)};
        {From, ?MSG_CHAT_LOG, Msg} ->
            {Logs, AckInfo} = chat_log(Msg),
            From ! {?MSG_CHAT_LOG, Logs},
            From ! {ack, AckInfo}
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
    ack(?CODE_ERROR_INTERVAL, "can't chat to self");
chat_private(From, {SrcAccInfo, DstAcc, Content}) ->
    SrcAcc= SrcAccInfo#account_info.account,
    ChatLog = #chat_log{src=SrcAcc, dst=DstAcc, chat_time=timestamp(), content=Content, type=private},
    add_chat_log(SrcAcc, ChatLog),
    add_chat_log(DstAcc, ChatLog),
    chat_push(From, SrcAcc, ChatLog),
    chat_push(From, DstAcc, ChatLog),
    ack(?CODE_OK, "chat private success").

chat_global(From, {SrcAccInfo, Content}) ->
    SrcAcc = SrcAccInfo#account_info.account,
    ChatLog = #chat_log{src=?GLOBAL_ACCOUNT, dst=SrcAcc, chat_time=timestamp(), content=Content, type=global},
    %全局聊天单独保存%
    add_chat_log(?GLOBAL_ACCOUNT, ChatLog),
    chat_push(From, SrcAcc, ChatLog),
    ack(?CODE_OK, "chat global success").


chat_log({AccInfo}) ->
    Acc = AccInfo#account_info.account,
    ChatInfos = db:find_one(chat_info, fun(ChatInfo) -> 
        ChatAcc = ChatInfo#chat_info.account,
        ChatAcc =:= Acc orelse ChatAcc =:= ?GLOBAL_ACCOUNT
    end), 
    {lists:foldl(fun(ChatInfo, Logs) -> Logs ++ ChatInfo#chat_info.logs end, [], ChatInfos), ack(?CODE_OK, "get chat log success")}.