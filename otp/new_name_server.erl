-module(new_name_server).
-export([init/0, add/2, find/1, handle/2, all_names/0, delete/1]).
-import(server3, [rpc/2]).

add(Name, Place) -> rpc(name_server, {add, Name, Place}).
find(Name) -> rpc(name_server, {find, Name}).
all_names() -> rpc(name_server, allNames).
delete(Name) -> rpc(name_server, {delete, Name}).

%% 回调方法
init() -> dict:new().
handle({add, Name, Place}, Dict) -> 
    {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) -> 
    {dict:find(Name, Dict), Dict};
handle({delete, Name}, Dict) -> 
    {dict:erase(Name, Dict)};
handle(allNames, Dict) -> 
    {dict:fetch_keys(Dict), Dict}.
