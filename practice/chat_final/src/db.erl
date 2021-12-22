-module(db).
-behaviour(gen_server).
-include("data.hrl").
-include("db.hrl").

%% API
-export([start_link/1]).
-export([record_2_map/2, map_2_record/2]).
-export([insert_one/2, update/3, find_account_info/1, find_chat_info/1, find_friend_info/1, update_raw/3, find_all_chat_info/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Conn) ->
    gen_server:start_link({local, get_db()}, ?MODULE, [Conn], []).

init([Conn]) ->
    {ok, #{conn=>Conn}}.

-spec handle_call('stop' | {'find_all', _, _} | {'find_one', _, _} | {'insert', _, _} | {'update', _, _, _}, _, _) -> {'reply', _, #{'conn':=_,  _=>_}} | {'stop', 'normal', 'stopped', _}.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({insert, Table, Docs}, _From, #{conn:=Conn}=State) ->
    Reply = mongo_api:insert(Conn, Table, Docs),
    {reply, Reply, State};
handle_call({update, Table, Filter, Doc}, _From, #{conn:=Conn}=State) ->
    Reply = mongo_api:update(Conn, Table, Filter, Doc, #{}),
    {reply, Reply, State};
handle_call({find_one, Table, Filter}, _From, #{conn:=Conn}=State) ->
    Reply = mongo_api:find_one(Conn, Table, Filter, #{}),
    {reply, Reply, State};
handle_call({find_all, Table, Filter}, _From, #{conn:=Conn}=State) ->
    {ok, Cursor} = mongo_api:find(Conn, Table, Filter, #{}),
    {reply, Cursor, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_db() ->
    ?MODULE.

insert_one(Table, Doc) ->
    BTable = list_to_binary(Table),
    gen_server:call(get_db(), {insert, BTable, [Doc]}).

update(Table, Filter, Doc) ->
    BTable = list_to_binary(Table),
    gen_server:call(get_db(), {update, BTable, Filter, #{'$set'=>Doc}}).

update_raw(Table, Filter, Raw) ->
    BTable = list_to_binary(Table),
    gen_server:call(get_db(), {update, BTable, Filter, Raw}).

% -spec get_record_info('fields' | 'size', 'account_info' | 'chat_info' | 'chat_log' | 'friend_info' | 'friend_simple' | 'user_info') -> [atom(), ...] | 3 | 6 | 9.
get_record_info(fields, Name) ->
    case Name of 
        account_info -> record_info(fields, account_info);
        user_info-> record_info(fields, user_info);
        chat_log-> record_info(fields, chat_log);
        chat_info-> record_info(fields, chat_info);
        friend_info -> record_info(fields, friend_info);
        friend_simple-> record_info(fields, friend_simple)
    end;

get_record_info(size, Name) ->
    case Name of 
        account_info -> record_info(size, account_info);
        user_info-> record_info(size, user_info);
        chat_log-> record_info(size, chat_log);
        chat_info-> record_info(size, chat_info);
        friend_info -> record_info(size, friend_info);
        friend_simple-> record_info(size, friend_simple)
    end.


get_map_item(K, M) ->
    BKey = list_to_binary(atom_to_list(K)),
    V = maps:get(BKey, M, undefined),
    if 
        is_binary(V) -> binary_to_list(V);
        is_map(V) -> map_2_record(V, K);
        true -> V
    end.

map_2_record(M, RecordName) -> 
    list_to_tuple(lists:reverse(
        lists:foldl(fun(E, S) -> [get_map_item(E, M)|S] end, [RecordName], get_record_info(fields, RecordName)))).
    

convert_map_item(V) ->
    if 
        is_list(V) -> list_to_binary(V);
        true -> V
    end.

record_2_map(Record, RecordName) -> 
    lists:foldl(fun({I, E}, M) -> M#{list_to_binary(atom_to_list(E)) => convert_map_item(element(I, Record))} end, #{}, lists:zip(lists:seq(2, get_record_info(size, RecordName)), get_record_info(fields, RecordName))).


find_account_info(Account) ->
    case find_one("account", #{account => list_to_binary(Account), delete_time => 0}) of 
        undefined -> [];
        Result -> [map_2_record(Result, account_info)]
    end.

find_chat_info(Account) ->
    case find_one("chat", #{account => list_to_binary(Account)}) of 
        undefined -> [];
        Result -> 
            ChatInfo = map_2_record(Result, chat_info),
            [ChatInfo#chat_info{logs=lists:map(fun(E) -> map_2_record(E, chat_log) end, ChatInfo#chat_info.logs)}]
    end.
find_all_chat_info(Account, Global) ->
    case find_all("chat", #{'$or'=>[#{account => list_to_binary(Account)}, #{account=>list_to_binary(Global)}]}) of 
        undefined -> [];
        Result -> 
            lists:map(fun(OneResult) ->
                ChatInfo = map_2_record(OneResult, chat_info),
                ChatInfo#chat_info{logs=lists:map(fun(E) -> map_2_record(E, chat_log) end, ChatInfo#chat_info.logs)}
            end, Result)
    end.

find_friend_info(Account) ->
    case find_one("friend", #{account => list_to_binary(Account)}) of 
        undefined -> [];
        Result -> 
            FriendInfo = map_2_record(Result, friend_info),
            [FriendInfo#friend_info{friends=lists:map(fun(E) -> map_2_record(E, friend_simple) end, FriendInfo#friend_info.friends)}]
    end.

find_one(Table, Filter) ->
    BTable = list_to_binary(Table),
    gen_server:call(get_db(), {find_one, BTable, Filter}).

find_all(Table, Filter) ->
    BTable = list_to_binary(Table),
    Cursor = gen_server:call(get_db(), {find_all, BTable, Filter}),
    Reply = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Reply.

