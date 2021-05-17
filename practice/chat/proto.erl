-module(proto).
-export([decode/1, encode/2]).
-include("proto.hrl").

decode(Bin)->
    <<MsgId:16,Data/binary>> = Bin,
    case MsgId of 
        ?MSG_ACK-> {?MSG_ACK, decode_msg_ack(Data)};
        ?MSG_LOGIN -> {?MSG_LOGIN, decode_msg_login(Data)};
        ?MSG_REGISTER-> {?MSG_REGISTER, decode_msg_register(Data)};
        ?MSG_LOGOUT-> {?MSG_LOGOUT, decode_msg_logout(Data)};
        ?MSG_UNREGISTER-> {?MSG_UNREGISTER, decode_msg_unregister(Data)};
        ?MSG_ADD_FRIEND-> {?MSG_ADD_FRIEND, decode_msg_add_friend(Data)};
        ?MSG_SEARCH_FRIEND-> {?MSG_SEARCH_FRIEND, decode_msg_search_friend(Data)};
        ?MSG_FRIEND_INFO-> {?MSG_FRIEND_INFO, decode_msg_friend_info(Data)};
        ?MSG_LIST_FRIEND-> {?MSG_LIST_FRIEND, decode_msg_list_friend(Data)};
        ?MSG_REM_FRIEND-> {?MSG_REM_FRIEND, decode_msg_rem_friend(Data)};
        ?MSG_FRIEND_LIST-> {?MSG_FRIEND_LIST, decode_msg_friend_list(Data)}
    end.

encode(MsgId, Msg)->
    Data = case MsgId of 
        ?MSG_ACK-> encode_msg_ack(Msg);
        ?MSG_LOGIN -> encode_msg_login(Msg);
        ?MSG_REGISTER-> encode_msg_register(Msg);
        ?MSG_LOGOUT-> encode_msg_logout(Msg);
        ?MSG_UNREGISTER-> encode_msg_unregister(Msg);
        ?MSG_ADD_FRIEND-> encode_msg_add_friend(Msg);
        ?MSG_SEARCH_FRIEND-> encode_msg_search_friend(Msg);
        ?MSG_FRIEND_INFO-> encode_msg_friend_info(Msg);
        ?MSG_LIST_FRIEND-> encode_msg_list_friend(Msg);
        ?MSG_REM_FRIEND-> encode_msg_rem_friend(Msg);
        ?MSG_FRIEND_LIST-> encode_msg_friend_list(Msg)
    end,
    <<MsgId:16, Data/binary>>.

decode_string(Data) ->
    <<Len:8, Data1/binary>> = Data,
    <<Str:Len/binary, Data2/binary>> = Data1,
    {binary_to_list(Str), Data2}.


encode_string(Str) ->
    Data = list_to_binary(Str),
    <<(byte_size(Data)):8, Data/binary>>.

decode_integer(Data, Size) ->
    <<Number:Size, Data1/binary>> = Data,
    {Number, Data1}.

encode_integer(Number, Size) ->
    <<Number:Size>>.

decode_list(Data, ElementDecoder) ->
    <<Len:16, Data1/binary>> = Data,
    do_decode_list([], Data1, Len, ElementDecoder).

do_decode_list(L, Data, 0, _ElementDecoder) ->
    {L, Data};
do_decode_list(L, Data, Len, ElementDecoder) ->
    {Ele, Data1} = ElementDecoder(Data),
    do_decode_list([Ele|L], Data1, Len-1, ElementDecoder).

encode_list(L, ElementEncoder) ->
    Data = <<(length(L)):16>>,
    do_encode_list(Data, L, ElementEncoder).

do_encode_list(Data, [], _ElementEncoder) ->
    Data;
do_encode_list(Data, [Ele|L], ElementEncoder) ->
    do_encode_list(<<Data/binary, (ElementEncoder(Ele))/binary>>, L, ElementEncoder).

decode_msg_ack(Data) ->
    {Id, Data1} = decode_integer(Data, 32),
    {Code, Data2} = decode_integer(Data1, 32),
    {Info, Data3} = decode_string(Data2),
    {#sc_ack_msg{id=Id, code=Code, info=Info}, Data3}.

encode_msg_ack(Msg) ->
    #sc_ack_msg{id=Id, code=Code, info=Info} = Msg,
    <<(encode_integer(Id, 32))/binary, (encode_integer(Code, 32))/binary, (encode_string(Info))/binary>>.
 

decode_msg_login(Data) ->
    {Account, Data1} = decode_string(Data),
    {Passwd, Data2} = decode_string(Data1),
    {#cs_login_msg{account=Account, passwd=Passwd}, Data2}.

encode_msg_login(Msg) ->
    #cs_login_msg{account=Acc, passwd=Pass} = Msg,
    <<(encode_string(Acc))/binary, (encode_string(Pass))/binary>>.

 
decode_msg_logout(Data) ->
    {#cs_logout_msg{}, Data}.
encode_msg_logout(_Msg) ->
    <<>>.

decode_msg_register(Data) ->
    {Account, Data1} = decode_string(Data),
    {Passwd, Data2} = decode_string(Data1),
    {#cs_register_msg{account=Account, passwd=Passwd}, Data2}.

encode_msg_register(Msg) ->
    #cs_register_msg{account=Acc, passwd=Pass} = Msg,
    <<(encode_string(Acc))/binary, (encode_string(Pass))/binary>>.

decode_msg_unregister(Data) ->
    {#cs_unregister_msg{}, Data}.

encode_msg_unregister(_Msg) ->
    <<>>.

decode_msg_add_friend(Data) ->
    {AccFrd, Data1} = decode_string(Data),
    {#cs_add_friend_msg{account_friend=AccFrd}, Data1}.

encode_msg_add_friend(Msg) ->
    #cs_add_friend_msg{account_friend=AccFrd} = Msg,
    encode_string(AccFrd).


decode_msg_search_friend(Data) ->
    {AccFrd, Data1} = decode_string(Data),
    {#cs_search_friend_msg{account_friend=AccFrd}, Data1}.

encode_msg_search_friend(Msg) ->
    #cs_search_friend_msg{account_friend=AccFrd} = Msg,
    encode_string(AccFrd).


decode_msg_friend_info(Data) ->
    {AccFrd, Data1} = decode_string(Data),
    {FriendTm, Data2} = decode_integer(Data1, 32),
    {#sc_friend_info_msg{account=AccFrd, friend_time=FriendTm}, Data2}.

encode_msg_friend_info(Msg) ->
    #sc_friend_info_msg{account=AccFrd, friend_time=FriendTm} = Msg,
    <<(encode_string(AccFrd))/binary, (encode_integer(FriendTm, 32))/binary>>.

decode_msg_list_friend(Data) ->
    {#cs_list_friend_msg{}, Data}.

encode_msg_list_friend(_Msg) ->
    <<>>.

decode_msg_rem_friend(Data) ->
    {AccFrd, Data1} = decode_string(Data),
    {#cs_rem_friend_msg{account_friend=AccFrd}, Data1}.

encode_msg_rem_friend(Msg) ->
    #cs_rem_friend_msg{account_friend=AccFrd} = Msg,
    <<(encode_string(AccFrd))/binary>>.

decode_msg_friend_list(Data) ->
    DecodeFriendSimple = fun(FrdData)->
        {AccFrd, Data1} = decode_string(FrdData),
        {FriendTm, Data2} = decode_integer(Data1, 32),
        {#friend_simple{account=AccFrd, friend_time=FriendTm}, Data2}
    end,

    {FriendList, Data1} = decode_list(Data, DecodeFriendSimple),
    {#sc_friend_list_msg{friends=FriendList}, Data1}.

encode_msg_friend_list(Msg) ->
    #sc_friend_list_msg{friends=FriendList} = Msg,
    EncodeFriendSimple = fun(FrdSimple) ->
        #friend_simple{account=AccFrd, friend_time=FriendTm} = FrdSimple,
        <<(encode_string(AccFrd))/binary, (encode_integer(FriendTm, 32))/binary>>
    end,
    <<(encode_list(FriendList, EncodeFriendSimple))/binary>>.

