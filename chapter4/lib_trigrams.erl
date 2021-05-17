-module(lib_trigrams).
-export([make_ets_set/0, open/0, is_word1/2, close/1]).

for_eatch_trigram_in_the_english_language(F, A0) ->
    {ok, Bin0} = file:read_file("354984si.ngl.gz"),
    Bin = zlib:gunzip(Bin0),
    scan_word_list(binary_to_list(Bin), F, A0).


scan_word_list([], _, A) ->
    A;
scan_word_list(L, F, A) ->
    {Word, L1} = get_next_word(L, []),
    A1 = scan_trigrams([$\s|Word], F, A),
    scan_word_list(L1, F, A1).


get_next_word([$\r, $\n|T], L) -> {lists:reverse([$\s|L]), T};
get_next_word([H|T], L) -> get_next_word(T, [H|L]);
get_next_word([], L) -> {lists:reverse([$\s|L]), []}.

scan_trigrams([X, Y, Z], F, A) ->
    F([X, Y, Z], A);
scan_trigrams([X, Y, Z|T], F, A) ->
    A1 = F([X, Y, Z], A),
    scan_trigrams([Y, Z|T], F, A1);
scan_trigrams(_, _, A) ->
    A.

make_ets_ordered_set() -> make_a_set(ordered_set, "trigramsOS.tab").
make_ets_set() -> make_a_set(set, "trigramsS.tab").

make_a_set(Type, FileName) ->
    Tab = ets:new(table, [Type]),
    F = fun(Str, _) -> ets:insert(Tab, {list_to_binary(Str)}) end,
    for_eatch_trigram_in_the_english_language(F, 0),
    ets:tab2file(Tab, FileName),
    Size = ets:info(Tab, size),
    ets:delete(Tab),
    Size.

is_word(Tab, Str) -> is_word1(Tab, "\s"++Str++"\s").
is_word1(Tab, [_,_,_]=X) -> is_this_a_trigram(Tab, X);
is_word1(Tab, [A,B,C|D]) ->
    case is_this_a_trigram(Tab, [A,B,C]) of 
        true -> is_word1(Tab, [B, C|D]);
        false -> false
    end;
is_word1(_, _) ->
    false.

is_this_a_trigram(Tab, X) ->
    case ets:lookup(Tab, list_to_binary(X)) of 
        [] -> false;
        _ -> true
    end.
open() ->
    File = filename:join(filename:dirname(code:which(?MODULE)), "trigramsS.tab"),
    {ok, Tab} = ets:file2tab(File),
    Tab.
close(Tab) -> ets:delete(Tab).

