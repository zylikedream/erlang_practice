-module(service_chat).
-behaviour(gen_server).
-include("proto.hrl").
-include("data.hrl").
-define(GLOBAL_ACCOUNT, "@--global--@").

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({MsgId, Msg}, From, State) ->
    {Data, Ack} = handle_chat_msg(From, MsgId, Msg),
    {reply, common:pack_service_result(MsgId, Ack, Data), State}.

handle_cast({MsgId, Msg, From}, State) ->
    {Data, Ack} = handle_chat_msg(From, MsgId, Msg),
    gen_server:reply({From, async}, common:pack_service_result(MsgId, Ack, Data)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_chat_msg(From, MsgId, Msg) ->
    case MsgId of 
        ?MSG_CHAT_PRIVATE-> 
            handle_chat_private(From, Msg);
        ?MSG_CHAT_ALL-> 
            handle_chat_all(From, Msg);
        ?MSG_CHAT_LOG->
            handle_chat_log(Msg)
    end.

chat_push([], _Acc, _ChatLog) ->
    void;
chat_push(Server, Acc, ChatLog) ->
    gen_server:reply({Server, chat_push}, {Acc, ChatLog}).

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

handle_chat_private(_From, {SrcAccInfo, DstAcc, _Content}) when SrcAccInfo#account_info.account =:= DstAcc ->
    {{}, common:ack(?MSG_CHAT_PRIVATE, ?CODE_ERROR_INTERVAL, "can't chat to self")};
handle_chat_private(From, {SrcAccInfo, DstAcc, Content}) ->
    SrcAcc= SrcAccInfo#account_info.account,
    ChatLog = #chat_log{src=SrcAcc, dst=DstAcc, chat_time=common:timestamp(), content=Content, type=private},
    add_chat_log(SrcAcc, ChatLog),
    add_chat_log(DstAcc, ChatLog),
    chat_push(From, SrcAcc, ChatLog),
    chat_push(From, DstAcc, ChatLog),
    {{}, common:ack(?MSG_CHAT_PRIVATE, ?CODE_OK, "chat private success")}.

handle_chat_all(From, {SrcAccInfo, Content}) ->
    SrcAcc = SrcAccInfo#account_info.account,
    ChatLog = #chat_log{src=?GLOBAL_ACCOUNT, dst=SrcAcc, chat_time=common:timestamp(), content=Content, type=global},
    %????????????????????????%
    add_chat_log(?GLOBAL_ACCOUNT, ChatLog),
    chat_push(From, SrcAcc, ChatLog),
    {{}, common:ack(?MSG_CHAT_ALL, ?CODE_OK, "chat global success")}.


handle_chat_log({AccInfo}) ->
    Acc = AccInfo#account_info.account,
    ChatInfos = db:find_one(chat_info, fun(ChatInfo) -> 
        ChatAcc = ChatInfo#chat_info.account,
        ChatAcc =:= Acc orelse ChatAcc =:= ?GLOBAL_ACCOUNT
    end), 
    {lists:foldl(fun(ChatInfo, Logs) -> Logs ++ ChatInfo#chat_info.logs end, [], ChatInfos), common:ack(?MSG_CHAT_LOG, ?CODE_OK, "get chat log success")}.



