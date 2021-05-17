-module(chat_client).
-export([connect/0, login/3, register/3, logout/1, unregister/1, add_friend/2, search_friend/2, rem_friend/2, list_friend/1]).
-include("proto.hrl").

connect()->
    spawn(fun() -> 
        {ok, Socket} = gen_tcp:connect("localhost", 50001, [binary, {packet, 2}]),
        loop(Socket) 
    end).

loop(Socket) ->
    receive
        {_, send, Data} -> 
            gen_tcp:send(Socket, Data),
            loop(Socket);
        {tcp, Socket, Bin} -> 
            {MsgId, Msg} = proto:decode(Bin),
            case MsgId of 
                ?MSG_ACK ->
                    io:format("recv ack:~p~n", [Msg]);
                ?MSG_FRIEND_INFO ->
                    io:format("recv friend info:~p~n", [Msg]);
                ?MSG_FRIEND_LIST ->
                    io:format("recv friend list:~p~n", [Msg])
            end,
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed ~n", [Socket])
    end.

login(Client, Account, Passwd) ->
    Msg= #cs_login_msg{account=Account, passwd=Passwd},
    Client ! {self(), send, proto:encode(?MSG_LOGIN, Msg)}.

logout(Client) ->
    Msg= #cs_logout_msg{},
    Client ! {self(), send, proto:encode(?MSG_LOGOUT, Msg)}.
    
        
register(Client, Account, Passwd) ->
    Msg= #cs_register_msg{account=Account, passwd=Passwd},
    Client ! {self(), send, proto:encode(?MSG_REGISTER, Msg)}.


unregister(Client) ->
    Msg= #cs_unregister_msg{},
    Client ! {self(), send, proto:encode(?MSG_UNREGISTER, Msg)}.

add_friend(Client, Acc) ->
    Msg = #cs_add_friend_msg{account_friend=Acc},
    Client ! {self(), send, proto:encode(?MSG_ADD_FRIEND, Msg)}.

search_friend(Client, Acc) ->
    Msg = #cs_search_friend_msg{account_friend=Acc},
    Client ! {self(), send, proto:encode(?MSG_SEARCH_FRIEND, Msg)}.

rem_friend(Client, Acc) ->
    Msg = #cs_rem_friend_msg{account_friend=Acc},
    Client ! {self(), send, proto:encode(?MSG_REM_FRIEND, Msg)}.

list_friend(Client) ->
    Msg = #cs_list_friend_msg{},
    Client ! {self(), send, proto:encode(?MSG_LIST_FRIEND, Msg)}.

