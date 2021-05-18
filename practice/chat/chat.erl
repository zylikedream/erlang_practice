-module(chat).
-export([start/0, chat_log2chat_msg/1]).
-include("proto.hrl").
-define(GLOBAL_ACCOUNT, "@--global--@").

start() ->
    start_server(),
    start_friend_service(),
    start_chat_service().

start_friend_service() ->
    ets:new(friend, [set, public, named_table, {keypos, #friends_info.account}]),
    register(friend_service, spawn(fun() -> friend_loop() end)).

start_chat_service() ->
    ets:new(chat, [set, public, named_table, {keypos, #chat_info.account}]),
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
            {MsgId, {Msg, _}} = proto:decode(Bin),
            io:format("recv packet id:~w msg:~w~n", [MsgId, Msg]),
            Acc = handle_account_msg({Socket, Account, MsgId, Msg}),
            socket_loop(Socket, Acc);
        {tcp_closed, Socket}->
            io:format("socket ~w close ~n", [Socket]),
            disconnect(Account);
        {chat_push, Acc, ChatLog}->
            spawn(fun() -> handle_chat_push(Acc, ChatLog) end),
            socket_loop(Socket, Account)
    end.

ack(Code, Info) ->
    #ack{code=Code, info=Info}.

disconnect("") -> 
    void;
disconnect(Account) -> 
    ets:update_element(account, Account, {#account_info.logout_time, timestamp()}),
    ets:update_element(account, Account, {#account_info.socket, []}).

is_user_online(AccInfo) when AccInfo#account_info.socket =:= [] -> false;
is_user_online(_AccInfo) -> true.

handle_chat_push(Acc, ChatLog) ->
    case ChatLog#chat_log.type of  
       private ->
           chat_private_push(Acc, ChatLog);
       global ->
           chat_global_push(Acc, ChatLog)
    end.

chat_log2chat_msg([]) ->
    [];
chat_log2chat_msg(ChatLog) ->
    #chat_log{src=Src, dst=Dst, content=Content, chat_time=ChatTime, type=Type} = ChatLog,
    #sc_chat_msg{src=Src, dst=Dst, content=Content, chat_time=ChatTime, type=Type}.


chat_private_push(Acc, ChatLog) ->
    case ets:lookup(account, Acc) of
        [] ->
            void;
        [AccInfo] ->
            case is_user_online(AccInfo) of 
                true -> gen_tcp:send(AccInfo#account_info.socket, proto:encode(?MSG_CHAT_MSG, chat_log2chat_msg(ChatLog))); 
                false -> void
            end
    end.

chat_global_push(_Acc, ChatLog) ->
    lists:map(fun(AccInfo) ->  
            case is_user_online(AccInfo) of 
                true -> gen_tcp:send(AccInfo#account_info.socket, proto:encode(?MSG_CHAT_MSG, chat_log2chat_msg(ChatLog))); 
                false -> void
            end
        end,
        ets:tab2list(account)).


handle_account_msg({Socket, Account, MsgId, Msg}) ->
    {Acc, Ack} = case MsgId of
        ?MSG_LOGIN -> do_login(Socket, Account, Msg);
        ?MSG_REGISTER-> do_register(Socket, Msg);
        ?MSG_LOGOUT-> do_logout(Socket, Account, Msg);
        ?MSG_UNREGISTER-> do_unregister(Socket, Account, Msg);

        ?MSG_ADD_FRIEND -> do_add_friend(Socket, Account, Msg);
        ?MSG_SEARCH_FRIEND -> do_search_friend(Socket, Account, Msg);
        ?MSG_REM_FRIEND -> do_rem_friend(Socket, Account, Msg);
        ?MSG_LIST_FRIEND -> do_list_friend(Socket, Account, Msg);

        ?MSG_CHAT_ALL -> do_chat_all(Socket, Account, Msg);
        ?MSG_CHAT_PRIVATE -> do_chat_private(Socket, Account, Msg);
        ?MSG_CHAT_LOG-> do_chat_log(Socket, Account, Msg)
    end,
    io:format("handler Msg id:~p data:~p ack:~p~n", [MsgId, Msg, Ack]),
    % ack消息
    AckMsg =#sc_ack_msg{id=MsgId, code=Ack#ack.code, info=Ack#ack.info},
    gen_tcp:send(Socket, proto:encode(?MSG_ACK, AckMsg)),
    Acc.

handle_service_msg(Socket, Account) ->
    receive 
        {ack, Ack} -> 
            {Account, Ack};
        {?MSG_CHAT_LOG, Logs} ->
            io:format("Logs=~p~n", [Logs]),
            LogsMsg = #sc_chat_log{logs=lists:map(fun(Log) -> chat_log2chat_msg(Log) end, Logs)},
            gen_tcp:send(Socket, proto:encode(?MSG_CHAT_LOG_INFO, LogsMsg)),
            handle_service_msg(Socket, Account);
        {?MSG_SEARCH_FRIEND,  Friend} -> 
            FrdMsg = #sc_friend_info_msg{account=Friend#friend_simple.account, friend_time=Friend#friend_simple.friend_time},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_INFO, FrdMsg)),
            handle_service_msg(Socket, Account);
        {?MSG_LIST_FRIEND, Friends} -> 
            FriendsMsg = #sc_friend_list_msg{friends=Friends},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_LIST, FriendsMsg)),
            handle_service_msg(Socket, Account)
    after 3000 ->
        {Account, ack(?CODE_ERROR_INTERVAL, "timeout")}
    end.

do_chat_all(_Socket, "", _Msg) ->
    {"", ack(?CODE_ERROR_INTERVAL, "need login first")};
do_chat_all(Socket, Account, Msg) ->
    chat_service ! {self(), ?MSG_CHAT_ALL, {Account, Msg#cs_chat_all.content}},
    handle_service_msg(Socket, Account).

do_chat_private(_Socket, "", _Msg) ->
    {"", ack(?CODE_ERROR_INTERVAL, "need login first")};
do_chat_private(Socket, Account, Msg) ->
    chat_service ! {self(), ?MSG_CHAT_PRIVATE, {Account, Msg#cs_chat_private.dst, Msg#cs_chat_private.content}},
    handle_service_msg(Socket, Account).

do_chat_log(Socket, Account, _Msg) ->
    chat_service ! {self(), ?MSG_CHAT_LOG, {Account}},
    handle_service_msg(Socket, Account).


do_add_friend(_Socket, "", _Msg) ->
    {"", ack(?CODE_ERROR_INTERVAL, "need login first")};
do_add_friend(Socket, Account, Msg) ->
    friend_service ! {self(), ?MSG_ADD_FRIEND, {Account, Msg#cs_add_friend_msg.account_friend}},
    handle_service_msg(Socket, Account).

do_search_friend(_Socket, "", _Msg) ->
    {"", ack(?CODE_ERROR_INTERVAL, "need login first")};
do_search_friend(Socket, Account, Msg) ->
    friend_service ! {self(), ?MSG_SEARCH_FRIEND, {Account, Msg#cs_search_friend_msg.account_friend}},
    handle_service_msg(Socket, Account).

do_rem_friend(_Socket, "", _Msg) ->
    {"", ack(?CODE_ERROR_INTERVAL, "need login first")};
do_rem_friend(Socket, Account, Msg) ->
    friend_service ! {self(), ?MSG_REM_FRIEND, {Account, Msg#cs_rem_friend_msg.account_friend}},
    handle_service_msg(Socket, Account).


do_list_friend(_Socket, "", _Msg) ->
    {"", ack(?CODE_ERROR_INTERVAL, "need login first")};
do_list_friend(Socket, Account, _Msg) ->
    friend_service ! {self(), ?MSG_LIST_FRIEND, {Account}},
    handle_service_msg(Socket, Account).

do_login(Socket, Account, Msg) ->
    if 
        Account =/= "" -> {Account, ack(?CODE_OK, "already logined")};
        true->
            #cs_login_msg{account=Acc, passwd=Pass} = Msg,
            case ets:lookup(account, Acc) of 
                [] -> {"", ack(?CODE_ERROR_INTERVAL, "account not found. need register first")};
                [AccInfo] -> 
                    if 
                        AccInfo#account_info.passwd =/= Pass -> {"", "passwd or account invalid"};
                        true ->
                            io:format("account_info:~p~n", [AccInfo]),
                            ets:update_element(account, AccInfo#account_info.account, {#account_info.login_time, timestamp()}),
                            ets:update_element(account, AccInfo#account_info.account, {#account_info.server, self()}),
                            ets:update_element(account, AccInfo#account_info.account, {#account_info.socket, Socket}),
                            on_user_online(AccInfo),
                            {Acc, ack(?CODE_OK, "login success")}
                    end
            end
    end.


do_logout(_Socket, "", __Msg) ->
        {"", ack(?CODE_ERROR_INTERVAL, "logout failed. need login first")};
do_logout(Socket, Account, _Msg) ->
    case ets:lookup(account, Account) of 
        [] -> {"", ack(?CODE_ERROR_INTERVAL, "logout failed.not found account")};
        [_AccInfo] ->
            gen_tcp:close(Socket),
            {"", ack(?CODE_OK, "logout success")}
    end.

do_unregister(_Socket, "", _Msg) ->
    {"", ack(?CODE_ERROR_INTERVAL, "unregister failed. need login first")};
do_unregister(Socket, Account, _Msg) ->
    case ets:lookup(account, Account) of 
        [] -> {"", ack(?CODE_OK, "unregister success")};
        [_] ->
            ets:delete(account, Account),
            gen_tcp:close(Socket),
            {"", ack(?CODE_OK, "unregister success")}
    end.

do_register(_Socket, Msg) ->
    #cs_register_msg{account=Acc, passwd=Pass} = Msg,
    case ets:lookup(account, Acc) of 
        [] -> 
            ets:insert(account, #account_info{account=Acc, passwd=Pass, register_time=timestamp(), user_info=#user_info{account=Acc}}),
            {"", ack(?CODE_OK, "registersuccess")};
        _ -> {"", ack(?CODE_ERROR_INTERVAL, "already reistered")}
    end.

timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

on_user_online(_AccInfo) ->
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
    Friends = #friends_info{account=Acc, friends=[]},
    ets:insert(friend, Friends),
    Friends.

handle_add_friend({AccSelf, AccFrd}) when AccSelf =:= AccFrd->
    ack(?CODE_ERROR_INTERVAL, "Can't add self");
handle_add_friend({AccSelf, AccFrd})->
    Ack = inner_handle_add_friend({AccSelf, AccFrd}),
    if 
        Ack#ack.code =/= ?CODE_OK -> Ack;
        true -> inner_handle_add_friend({AccFrd, AccSelf})
    end.

inner_handle_add_friend({AccSelf, AccFrd})->
    Friends = case ets:lookup(friend, AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case ets:lookup(account, AccFrd) of
        [] -> ack(?CODE_ERROR_INTERVAL, "friend not exist");
        [_] ->
            case find_friend(Friends#friends_info.friends, AccFrd) of
                [] -> 
                    NewFriends = [#friend_simple{account=AccFrd, friend_time=timestamp()}|Friends#friends_info.friends],
                    ets:update_element(friend, AccSelf, {#friends_info.friends, NewFriends}),
                    ack(?CODE_OK, "add friend success");
                _ -> ack(?CODE_ERROR_INTERVAL, "already friend")
            end
    end.


handle_search_friend({AccSelf, AccFrd})->
    Friends = case ets:lookup(friend, AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case find_friend(Friends#friends_info.friends, AccFrd) of
        [] -> {#friend_simple{}, ack(?CODE_ERROR_INTERVAL, "not find")};
        Friend -> {Friend, ack(?CODE_OK, "find success")}
    end.


handle_rem_friend({AccSelf, AccFrd})->
    Ack = inner_handle_rem_friend({AccSelf, AccFrd}),
    if 
        Ack#ack.code =/= ?CODE_OK -> Ack;
        true -> inner_handle_rem_friend({AccFrd, AccSelf})
    end.

inner_handle_rem_friend({AccSelf, AccFrd})->
    Friends = case ets:lookup(friend, AccSelf) of 
        [] -> ack(?CODE_OK, "find friend info failed");
        [F] -> F
    end,
    case find_friend(Friends#friends_info.friends, AccFrd) of
        [] -> ack(?CODE_OK, "not friend no need remove");
        Friend -> 
            NewFriends = Friends#friends_info.friends--[Friend],
            ets:update_element(friend, AccSelf, {#friends_info.friends, NewFriends}),
            ack(?CODE_OK, "success")
    end.

handle_list_friend({AccSelf})->
    Friends = case ets:lookup(friend, AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    {Friends#friends_info.friends, ack(?CODE_OK, "success")}.

chat_loop()->
    receive
        {From, ?MSG_CHAT_PRIVATE, Msg} ->
            From ! {ack, chat_private(Msg)};
        {From, ?MSG_CHAT_ALL, Msg} ->
            From ! {ack, chat_global(Msg)};
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
    ets:insert(chat, ChatInfo),
    ChatInfo.

add_chat_log(Acc, ChatLog) ->
    ChatInfo = case ets:lookup(chat, Acc) of 
        [] -> default_chat(Acc);
        [C] -> C
    end,
    NewChatLog = [ChatLog|ChatInfo#chat_info.logs],
    ets:update_element(chat, Acc, {#chat_info.logs, NewChatLog}).

chat_private({Src, Dst, _Content}) when Src =:= Dst ->
    ack(?CODE_ERROR_INTERVAL, "can't chat to self");
chat_private({Src, Dst, Content}) ->
    case ets:lookup(account, Src) of 
        [] -> ack(?CODE_ERROR_INTERVAL, "cant' find src");
        [AccSrc] -> 
            case ets:lookup(account, Dst) of
                [] -> ack(?CODE_ERROR_INTERVAL, "can't find dst");
                [AccDst] ->
                    ChatLog = #chat_log{src=Src, dst=Dst, chat_time=timestamp(), content=Content, type=private},
                    add_chat_log(Src, ChatLog),
                    add_chat_log(Dst, ChatLog),
                    chat_push(AccSrc#account_info.server, Src, ChatLog),
                    chat_push(AccDst#account_info.server, Dst, ChatLog),
                    ack(?CODE_OK, "chat private success")
            end
    end.

chat_global({Src, Content}) ->
    case ets:lookup(account, Src) of 
        [] -> ack(?CODE_ERROR_INTERVAL, "cant' find src");
        [AccSrc] -> 
            ChatLog = #chat_log{src=?GLOBAL_ACCOUNT, dst=Src, chat_time=timestamp(), content=Content, type=global},
            %全局聊天单独保存%
            add_chat_log(?GLOBAL_ACCOUNT, ChatLog),
            chat_push(AccSrc#account_info.server, Src, ChatLog),
            ack(?CODE_OK, "chat global success")
    end.


chat_log({Acc}) ->
    ChatInfo = case ets:lookup(chat, Acc) of 
        [] -> default_chat(Acc);
        [C] -> C
    end,
    GlobalChatInfo = case ets:lookup(chat, ?GLOBAL_ACCOUNT) of 
        [] -> #chat_info{};
        [G] -> G
    end,
    {GlobalChatInfo#chat_info.logs ++ ChatInfo#chat_info.logs, ack(?CODE_OK, "get chat log success")}.