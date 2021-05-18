-module(db).
-export([start/0, init/0, insert/1, find_one/2, find_all/1, delete/2, 
create_table_account/0, create_table_chat/0, create_table_friend/0,
find_account_info/1, find_chat_info/1, find_friend_info/1, update/1, delete_account/1]).
-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").

start() ->
    mnesia:start().

init() ->
    start(),
    create_table_account(),
    create_table_friend(),
    create_table_chat().

create_table_account() ->
    mnesia:create_table(account_info, [{attributes, record_info(fields, account_info)}, {type,set}, {disc_copies, [node()]}]).

create_table_friend() ->
    mnesia:create_table(friend_info, [{attributes, record_info(fields, friend_info)}, {type,set}, {disc_copies, [node()]}]).

-spec create_table_chat() -> {'aborted', _} | {'atomic', 'ok'}.
create_table_chat() ->
    mnesia:create_table(chat_info, [{attributes, record_info(fields, chat_info)}, {type,set}, {disc_copies, [node()]}]).

insert(Row) ->
    F = fun() ->
        mnesia:write(Row)
    end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


update(Row) ->
    insert(Row).

find_account_info(Account) ->
    find_one(account_info, fun(AccInfo) -> AccInfo#account_info.account =:= Account andalso AccInfo#account_info.delete_time =:= 0 end).

find_chat_info(Account) ->
    find_one(chat_info, fun(ChatInfo) -> ChatInfo#chat_info.account =:= Account end).

find_friend_info(Account) ->
    find_one(friend_info, fun(FriendInfo) -> FriendInfo#friend_info.account =:= Account end).

find_one(Table, Filter) ->
     F = fun() ->
		Q = qlc:q([Row || Row <- mnesia:table(Table), Filter(Row)]),
		qlc:e(Q)
	end,
    {atomic, Row} = mnesia:transaction(F),
    Row.

find_all(Table) ->
     F = fun() ->
		Q = qlc:q([Row || Row <- mnesia:table(Table)]),
		qlc:e(Q)
	end,
    {atomic, Rows} = mnesia:transaction(F),
    Rows.

delete(Table, Account) ->
    Oid = {Table, Account},
    F = fun() ->
        mnesia:delete(Oid)
    end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

delete_account(Account) ->
    delete(account_info, Account).


