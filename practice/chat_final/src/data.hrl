-record(user_info, {account="", name=""}).
-record(account_info, {account="", passwd="", register_time=0, login_time=0, logout_time=0, socket=[], user_info, delete_time=0}).

-record(friend_simple, {account="", friend_time=0}).
-record(friend_info, {account="", friends}).

-record(chat_log, { src="", dst="", chat_time=0, content="", type=private}).
-record(chat_info, {account="", logs=[]}).
