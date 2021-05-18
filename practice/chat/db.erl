-module(db).
-export([start/0, init/0, insert/1, find_one/2, find_all/1, delete/2, 
create_table_account/0, create_table_chat/0, create_table_friend/0,
find_account_info/1, find_chat_info/1, find_friend_info/1]).
-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").

start() ->
    mnesia:start().

init() ->
    create_table_account().

create_table_account() ->
    mnesia:create_table(account_info, [{attributes, record_info(fields, account_info)}]).

create_table_friend() ->
    mnesia:create_table(friend_info, [{attributes, record_info(fields, friend_info)}]).

create_table_chat() ->
    mnesia:create_table(chat_info, [{attributes, record_info(fields, chat_info)}]).

insert(Row) ->
    F = fun() ->
        mnesia:write(Row)
    end,
    mnesia:transaction(F).

find_account_info(Account) ->
    find_one(account_info, fun(AccInfo) -> AccInfo#account_info.account == Account end).

find_chat_info(Account) ->
    find_one(chat_info, fun(ChatInfo) -> ChatInfo#chat_info.account == Account end).

find_friend_info(Account) ->
    find_one(friend_info, fun(FriendInfo) -> FriendInfo#chat_info.account == Account end).

find_one(Table, Filter) ->
     F = fun() ->
		Q = qlc:q([Row || Row <- mnesia:table(Table), Filter(Row)]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).

find_all(Table) ->
     F = fun() ->
		Q = qlc:q([Row || Row <- mnesia:table(Table)]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).

delete(Table, Account) ->
    Oid = {Table, Account},
    F = fun() ->
        mnesia:delete(Oid)
    end,
    mnesia:transaction(F).

