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
-define(MSG_CHAT_ALL, 2002).
-define(MSG_CHAT_LOG, 2003).
-define(MSG_CHAT_LOG_INFO, 2004).
-define(MSG_CHAT_MSG, 2005).

-define(CODE_ERROR_INTERVAL, -1).
-define(CODE_OK, 0).

-record(sc_ack_msg, {id::integer(), code::integer(), info::string()}).
-record(cs_login_msg, {account::string(), passwd::string()}).
-record(cs_logout_msg, {}).
-record(cs_register_msg, {account::string(), passwd::string()}).
-record(cs_unregister_msg, {}).

-record(cs_add_friend_msg, {account_friend::string()}).
-record(cs_search_friend_msg, {account_friend::string()}).
-record(sc_friend_info_msg, {account::string(), friend_time::integer()}).
-record(cs_rem_friend_msg, {account_friend::string()}).
-record(cs_list_friend_msg, {}).
-record(sc_friend_list_msg, {friends::list(sc_friend_info_msg)}).

-record(cs_chat_private, {dst::string(), content::string()}).
-record(cs_chat_all, {content::string()}).
-record(cs_chat_log, {}).
-record(sc_chat_msg, {src::string(), dst::string(), chat_time::integer(), content::string(), type::private|global}).
-record(sc_chat_log, {logs::list(sc_chat_msg)}).

