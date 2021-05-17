-module(practice).
-export([zip/2, screen_lock_count/0, screen_lock_gap/2, in_lock/2, ets_test/0, ets_insert/2, ets_delete/2, ets_lookup/2]).

% ---------------------------zip-------------------------------
zip(A, B) ->
    do_zip(A, B, []).
    
do_zip([], _, L) ->
    lists:reverse(L);
do_zip(_, [], L)->
    lists:reverse(L);
do_zip([H1|A1], [H2|B1], L)->
    do_zip(A1, B1, [[H1, H2]|L]).
% ---------------------------zip-------------------------------

%----------------------------screen_lock----------------------

screen_lock_count() ->
    % 计算长度为[4,9]的所有解锁图案之和
    lists:sum(lists:map(fun(X) -> do_screen_lock_count(X, [], lists:seq(1, 9)) end, lists:seq(4, 9))).

% 计算解锁数字的数量
% N- 数字长度
% Lock-解锁数字集合
% L-剩余数字集合
do_screen_lock_count(0, _, _) ->
    1;

do_screen_lock_count(N, Lock, L) ->
    lists:sum([do_screen_lock_count_one(N-1, Cur, Lock, L--[Cur]) || Cur <-L]).

% 计算数字Cur作为解锁数字的数量
do_screen_lock_count_one(N, Cur, [], L)->
    do_screen_lock_count(N, [Cur], L);

do_screen_lock_count_one(N, Cur, Lock, L)->
    [Last|_] = Lock,
    % 当前数字和上一个数字是否有跳跃
    Gap = screen_lock_gap(Last, Cur),
    % 判断跳跃的字符之前是否已经包含了
    case Gap of 
        0 -> do_screen_lock_count(N, [Cur|Lock], L);
        _ ->  case in_lock(Gap, Lock) of
            % 如果包含了，就表示当前数字是合法
            true -> do_screen_lock_count(N, [Cur|Lock], L);
            false -> 0
            end
    end.

% 某个数字是否在解锁图案中
in_lock(_, []) -> false;
in_lock(P, [N|_]) when P =:= N ->
    true;
in_lock(P, [_|Lock]) ->
    in_lock(P, Lock).

% 数字直接的间隔
screen_lock_gap(X, Y) ->
    case {X, Y} of 
        {1, 3} -> 2;
        {1, 7} -> 4;
        {1, 9} -> 5;
        {2, 8} -> 5;
        {3, 1} -> 2;
        {3, 9} -> 6;
        {3, 7} -> 5;
        {4, 6} -> 5;
        {6, 4} -> 5;
        {7, 1} -> 4;
        {7, 3} -> 5;
        {7, 9} -> 8;
        {8, 2} -> 5;
        {9, 1} -> 5;
        {9, 3} -> 6;
        {9, 7} -> 8;
        {_, _} -> 0
    end.

%----------------------------screen_lock----------------------

%-----------------------------ets_test------------------------
ets_test() ->
    Modes = [set, ordered_set, bag, duplicate_bag],
    % Modes = [set],
    lists:foreach(fun ets_mode_test/1, Modes).

ets_mode_test(Mode) ->
    Table = ets:new(test, [Mode]),
    ets_mode_insert_test(Table),
    ets_mode_lookup_test(Table),
    ets_mode_delete_test(Table).


ets_test_format(Table, Op, Cost, Count) ->
    Mode = ets:info(Table, type),
    io:format("Ets ~s ~w:~w cost:~wus, per:~wus~n", [Mode, Op, Count, Cost, Cost/Count]).

ets_insert(Table, L) ->
    lists:foreach(fun(I) -> ets:insert(Table, {I}) end, L).
ets_mode_insert_test(Table) ->
    Count = 1000000,
    L = [rand:uniform(Count) || _ <- lists:seq(1, Count)],
    {Cost, _} = timer:tc(?MODULE, ets_insert, [Table,  L]),
    ets_test_format(Table, insert, Cost, Count).

ets_lookup(Table, L) ->
    lists:foreach(fun(K) -> ets:lookup(Table, K) end, L).

ets_mode_lookup_test(Table) ->
    L = ets:tab2list(Table),
    Count = length(L),
    {Cost, _} = timer:tc(?MODULE, ets_lookup, [Table, L]),
    ets_test_format(Table, lookup, Cost, Count).

ets_delete(Table, L) ->
    lists:foreach(fun(K) -> ets:delete(Table, K) end, L).

ets_mode_delete_test(Table) ->
    L = ets:tab2list(Table),
    Count = length(L),
    {Cost, _} = timer:tc(?MODULE, ets_delete, [Table, L]),
    ets_test_format(Table, delete, Cost, Count).
%-----------------------------ets_test------------------------