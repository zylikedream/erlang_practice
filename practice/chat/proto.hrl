-define(MSG_ACK, 0000).

-define(MSG_LOGIN, 0001).
-define(MSG_LOGOUT, 0002).
-define(MSG_REGISTER, 0003).
-define(MSG_UNREGISTER, 0004).

-define(MSG_SEARCH_FRIEND, 1001).
-define(MSG_ADD_FRIEND, 1002).
-define(MSG_LIST_FRIEND, 1003).
-define(MSG_REM_FRIEND, 1004).
-define(MSG_FRIEND_INFO, 1005).
-define(MSG_FRIEND_LIST, 1006).

-define(MSG_CHAT_PRIVATE, 2001).
-define(MSG_CHAT_ROOM, 2003).
-define(MSG_CHAT_ALL, 2004).
-define(MSG_CHAT_ROOM_INFO, 2005).
-define(MSG_CHAT_GROUP, 2006).
-define(MSG_CHAT_ROOM_CREATE, 2007).
-define(MSG_CHAT_ROOM_JOIN, 2008).
-define(MSG_CHAT_ROOM_QUIT, 2009).

-record(sc_ack_msg, {id, code, info}).
-record(cs_login_msg, {account, passwd}).
-record(cs_logout_msg, {}).
-record(cs_register_msg, {account, passwd}).
-record(cs_unregister_msg, {}).

-record(cs_add_friend_msg, {account_friend}).
-record(cs_search_friend_msg, {account_friend}).
-record(sc_friend_info_msg, {account, friend_time}).
-record(cs_rem_friend_msg, {account_friend}).
-record(cs_list_friend_msg, {}).
% 服务器响应的消息
-record(sc_friend_list_msg, {friends}).

-record(user_info, {account="", chat_time=0, login_time=0, logout_time=0}).
-record(account_info, {account="", passwd="", register_time=0, socket=[], user_info}).
-record(friend_simple, {account="", friend_time=0}).
-record(friends_info, {account="", friends}).

-record(chat_message, {id="", src="", dst="", chat_time=0, content=""}).
-record(chat_room_member, {account="", role=""}).
-record(chat_room, {id="", type="", create_time=0, chat_history=[], members=[]}).
-record(chats_info, {account="", rooms=[]}).
