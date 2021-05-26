-module(service_friend).
-behaviour(gen_server).
-include("proto.hrl").
-include("data.hrl").

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({MsgId, Msg}, _From, State) ->
    {Data, Ack} = handle_friend_msg(MsgId, Msg),
    {reply, common:pack_service_result(MsgId, Ack, Data), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_friend_msg(MsgId, Msg) ->
    case MsgId of 
        ?MSG_ADD_FRIEND -> 
            handle_add_friend(Msg);
        ?MSG_SEARCH_FRIEND -> 
            handle_search_friend(Msg);
        ?MSG_REM_FRIEND ->
            handle_rem_friend(Msg);
        ?MSG_LIST_FRIEND -> 
            handle_list_friend(Msg)
    end.

find_friend([], _) -> [];
find_friend([F|_], Acc) when F#friend_simple.account =:= Acc -> F;
find_friend([_|Friends], Acc) -> find_friend(Friends, Acc).

default_friend(Acc) ->
    Friends = #friend_info{account=Acc, friends=[]},
    db:insert_one("friend", #{account=>list_to_binary(Acc), friends=>[]}),
    Friends.

handle_add_friend({SelfAccInfo, FrdAccInfo}) when SelfAccInfo#account_info.account =:= FrdAccInfo#account_info.account ->
    {{}, common:ack(?MSG_ADD_FRIEND, ?CODE_ERROR_INTERVAL, "Can't add self")};
handle_add_friend({SelfAccInfo, FrdAccInfo})->
    Ack = inner_handle_add_friend({SelfAccInfo, FrdAccInfo}),
    if 
        Ack#sc_ack_msg.code =/= ?CODE_OK -> {{}, Ack};
        true -> {{}, inner_handle_add_friend({FrdAccInfo, SelfAccInfo})}
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
            NewFriend = #friend_simple{account=AccFrd, friend_time=common:timestamp()},
            db:update_raw("friend", #{account=>list_to_binary(AccSelf)}, #{'$push'=>#{friends=>db:record_2_map(NewFriend, friend_simple)}}),
            common:ack(?MSG_ADD_FRIEND, ?CODE_OK, "add friend success");
        _ -> common:ack(?MSG_ADD_FRIEND, ?CODE_ERROR_INTERVAL, "already friend")
    end.


handle_search_friend({SelfAccInfo, AccFrd})->
    AccSelf = SelfAccInfo#account_info.account,
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case find_friend(Friends#friend_info.friends, AccFrd) of
        [] -> {#friend_simple{}, common:ack(?MSG_SEARCH_FRIEND, ?CODE_ERROR_INTERVAL, "not find")};
        Friend -> {Friend, common:ack(?MSG_SEARCH_FRIEND, ?CODE_OK, "find success")}
    end.


handle_rem_friend({SelfAccInfo, AccFrd})->
    AccSelf = SelfAccInfo#account_info.account,
    Ack = inner_handle_rem_friend({AccSelf, AccFrd}),
    if 
        Ack#sc_ack_msg.code =/= ?CODE_OK -> {{}, Ack};
        true -> {{}, inner_handle_rem_friend({AccFrd, AccSelf})}
    end.

inner_handle_rem_friend({AccSelf, AccFrd})->
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    case find_friend(Friends#friend_info.friends, AccFrd) of
        [] -> common:ack(?MSG_REM_FRIEND, ?CODE_OK, "not friend no need remove");
        Friend -> 
            db:update_raw("friend", #{account=>list_to_binary(AccSelf)}, #{'$pull'=>#{friends=>db:record_2_map(Friend, friend_simple)}}),
            common:ack(?MSG_REM_FRIEND, ?CODE_OK, "success")
    end.

handle_list_friend({SelfAccInfo})->
    AccSelf = SelfAccInfo#account_info.account,
    Friends = case db:find_friend_info(AccSelf) of 
        [] -> default_friend(AccSelf);
        [F] -> F
    end,
    {Friends#friend_info.friends, common:ack(?MSG_LIST_FRIEND, ?CODE_OK, "success")}.
