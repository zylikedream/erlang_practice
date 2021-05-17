-module(chat).
-export([start/0]).
-include("proto.hrl").

start() ->
    % Friend = spawn(fun() -> friend_loop() end),
    % Chat = spawn(fun() -> chat_loop() end),
    Server = start_server(),
    start_friend_service(),
    start_chat_service(Server).

start_friend_service() ->
    ets:new(friend, [set, public, named_table, {keypos, #friends_info.account}]),
    register(friend_service, spawn(fun() -> friend_loop() end)).

start_chat_service(Server) ->
    ets:new(user_chat, [set, public, named_table, {keypos, #chats_info.account}]),
    ets:new(chat_room, [set, public, named_table, {keypos, #chat_room.id}]),
    register(chat_service, spawn(fun() -> chat_loop(Server) end)).


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
            io:format("socket ~w close ~n", [Socket])
    end.

handle_account_msg({Socket, Account, MsgId, Msg}) ->
    {Acc, Resp} = case MsgId of
        ?MSG_LOGIN -> do_login(Socket, Account, Msg);
        ?MSG_REGISTER-> do_register(Msg);
        ?MSG_LOGOUT-> do_logout(Account, Msg);
        ?MSG_UNREGISTER-> do_unregister(Socket, Account, Msg);
        ?MSG_ADD_FRIEND -> do_add_friend(Account, Msg);
        ?MSG_SEARCH_FRIEND -> do_search_friend(Socket, Account, Msg);
        ?MSG_REM_FRIEND -> do_rem_friend(Account, Msg);
        ?MSG_LIST_FRIEND -> do_list_friend(Socket, Account, Msg)
    end,
    io:format("handler Msg id:~w data:~w result:~s~n", [MsgId, Msg, Resp]),
    % ack消息
    Ack =#sc_ack_msg{id=MsgId, code=0, info=Resp},
    gen_tcp:send(Socket, proto:encode(?MSG_ACK, Ack)),
    Acc.

do_add_friend(Account, Msg) ->
    friend_service ! {self(), ?MSG_ADD_FRIEND, {Account, Msg#cs_add_friend_msg.account_friend}},
    receive 
        Resp -> {Account, Resp}
    after 3000 ->
        {Account, "timeout"}
    end.

do_search_friend(Socket, Account, Msg) ->
    friend_service ! {self(), ?MSG_SEARCH_FRIEND, {Account, Msg#cs_search_friend_msg.account_friend}},
    receive 
        Friend -> 
            FrdMsg = #sc_friend_info_msg{account=Friend#friend_simple.account, friend_time=Friend#friend_simple.friend_time},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_INFO, FrdMsg)),
            {Account, "success"}
    after 3000 ->
        {Account, "timeout"}
    end.


do_rem_friend(Account, Msg) ->
    friend_service ! {self(), ?MSG_REM_FRIEND, {Account, Msg#cs_rem_friend_msg.account_friend}},
    receive 
        Resp-> {Account, Resp}
    after 3000 ->
        {Account, "timeout"}
    end.


do_list_friend(Socket, Account, _Msg) ->
    friend_service ! {self(), ?MSG_LIST_FRIEND, {Account}},
    receive 
        Friends -> 
            FriendsMsg = #sc_friend_list_msg{friends=Friends},
            gen_tcp:send(Socket, proto:encode(?MSG_FRIEND_LIST, FriendsMsg)),
            {Account, "success"}
    after 3000 ->
        {Account, "timeout"}
    end.

do_login(Socket, Account, Msg) ->
    if 
        Account =/= "" -> {Account, "already logined"};
        true->
            #cs_login_msg{account=Acc, passwd=Pass} = Msg,
            case ets:lookup(account, Acc) of 
                [] -> {"", "account not found. need register first"};
                [AccInfo] -> 
                    if 
                        AccInfo#account_info.passwd =/= Pass -> {"", "passwd or account invalid"};
                        true ->
                            io:format("account_info:~w~n", [AccInfo]),
                            ets:update_element(account, Acc#account_info.account, {#account_info.socket, Socket}),
                            on_user_online(AccInfo#account_info.user_info),
                            {Acc, "login success"}
                    end
            end
    end.


do_logout(Account, _Msg) ->
    if 
        Account =:= "" -> {"", "logout failed. need login_first"};
        true ->
        case ets:lookup(account, Account) of 
            [] -> {"", "logout failed.not found account"};
            [AccInfo] ->
                ets:update_element(account, AccInfo#account_info.account, {#account_info.user_info, AccInfo#account_info.user_info#user_info{logout_time=timestamp()}}),
                %todo 广播玩家登出
                {"", "logout success"}
        end
    end.

do_unregister(Socket, Account, _Msg) ->
    if 
        Account =:= "" -> {"", "unregister failed. need login_first"};
        true ->
        case ets:lookup(account, Account) of 
            [] -> {"", "unregister success"};
            [_] ->
                ets:delete(account, Account),
                %todo 广播玩家登出
                gen_tcp:close(Socket),
                {"", "unregister success"}
        end
    end.


do_register(Msg) ->
    #cs_register_msg{account=Acc, passwd=Pass} = Msg,
    case ets:lookup(account, Acc) of 
        [] -> 
            ets:insert(account, #account_info{account=Acc, passwd=Pass, register_time=timestamp(), user_info=#user_info{account=Acc}}),
            {"", "registersuccess"};
        _ -> {"", "already reistered"}
    end.

timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

on_user_online(User) ->
    UserNew = User#user_info{login_time=timestamp()},
    ets:update_element(account, User#user_info.account, {#account_info.user_info, UserNew}).
    % todo 广播上线消息

friend_loop() ->
    receive
        {From, ?MSG_ADD_FRIEND, Msg} -> 
            From ! handle_add_friend(Msg);
        {From, ?MSG_SEARCH_FRIEND, Msg} -> 
            From ! handle_search_friend(Msg);
        {From, ?MSG_REM_FRIEND, Msg} -> 
            From ! handle_rem_friend(Msg);
        {From, ?MSG_LIST_FRIEND, Msg} -> 
            From ! handle_list_friend(Msg)
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
    "Can't add self";
handle_add_friend({AccSelf, AccFrd})->
    Friends = case ets:lookup(friend, AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case ets:lookup(account, AccFrd) of
        [] -> "friend not exist";
        [_] ->
            case find_friend(Friends#friends_info.friends, AccFrd) of
                [] -> 
                    NewFriends = [#friend_simple{account=AccFrd, friend_time=timestamp()}|Friends#friends_info.friends],
                    ets:update_element(friend, AccSelf, {#friends_info.friends, NewFriends}),
                    "add friend success";
                _ -> "already friend"
            end
    end.

handle_search_friend({AccSelf, AccFrd})->
    Friends = case ets:lookup(friend, AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case find_friend(Friends#friends_info.friends, AccFrd) of
        [] -> #friend_simple{};
        Friend -> Friend
    end.


handle_rem_friend({AccSelf, AccFrd})->
    Friends = case ets:lookup(friend, AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case find_friend(Friends#friends_info.friends, AccFrd) of
        [] -> "success";
        Friend -> 
            NewFriends = Friends#friends_info.friends--[Friend],
            ets:update_element(friend, AccSelf, {#friends_info.friends, NewFriends}),
            "success"
    end.

handle_list_friend({AccSelf})->
    Friends = case ets:lookup(friend, AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    Friends#friends_info.friends.


chat_loop(Server)->
    receive
        {broadcast, Members, Msg} ->
            Server ! {self(), send_to_user, Members, Msg}
    end.

chat_gen_private_room_id(User1, User2) when User1 < User2 ->
    User1 ++ "_" + User2;
chat_gen_private_room_id(User1, User2) ->
    User2 ++ "_" + User1.

chat_gen_group_room_id(Adminer) ->
    Adminer ++ integer_to_list(timestamp()).

chat_create_room({Args, Type}) ->
    RoomId = case Type of 
        private ->  %私聊房间
            {User1, User2} = Args,
            chat_gen_private_room_id(User1, User2);
        group->
            {Adminer} = Args,
            chat_gen_group_room_id(Adminer)
    end,
    case ets:lookup(chat_room, RoomId) of
        [] ->
            case Type of 
                private ->
                    {UserSrc, UserDst} = Args,
                    Members = [#chat_room_member{account=UserSrc, role=admin}, #chat_room_member{account=UserDst, role=admin}];
                    UserRoom=[RoomId|]
                    ets:update_element((user_chat, UserSrc, {#chats_info.rooms, [RoomId|]}),
                group->
                    {User} = Args,
                    Members = [#chat_room_member{account=User, role=admin}]
            end,
            Room = #chat_room{id=RoomId, type=Type, create_time=timestamp(), members=Members},

            ets:insert(chat_room, Room),
            {Room, "create room success"};
        _ ->
            {[], "create room failed! room exists"}
   end.


find_member([], _) -> [];
find_member([M|_], Acc) when M#chat_room_member.account =:= Acc -> M;
find_member([_|Members], Acc) -> find_member(Members, Acc).

chat_broadcast_msg(Members, ChatMsg) ->
    self() ! {Members, ChatMsg}.


chat_to_room({RoomId, {Src, Dst, Content}}) ->
    case ets:lookup(chat_room, RoomId) of
        [] -> "can't find room";
        [Room] -> 
            case find_member(Room#chat_room.members, Src) =:= [] orelse (Dst =/= "" andalso find_member(Room#chat_room.members, Dst) =:= []) of 
                true -> "not room member";
                _ ->
                    ChatMsg = #chat_message{id=RoomId, src=Src, dst=Dst, content=Content, chat_time=timestamp()},
                    History = [ChatMsg|Room#chat_room.chat_history],
                    ets:update_element(chat_room, RoomId, {#chat_room.chat_history, History}),
                    self() ! {broadcast, Room#chat_room.members, ChatMsg},
                    chat_broadcast_msg(Room#chat_room.members, ChatMsg),
                    "chat_to_room success"
            end
end.
