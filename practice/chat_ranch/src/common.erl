-module(common).
-export([pack_service_result/3, ack/3, timestamp/0, unpack_service_result/1]).
-include("proto.hrl").

pack_service_result(MsgId, Ack, Data) ->
    {service, MsgId, {ack, Ack}, {data, Data}}.
unpack_service_result(Msg) ->
    {service, MsgId, {ack, Ack}, {data, Data}} = Msg,
    {MsgId, Ack, Data}.

ack(MsgId, Code, Info) ->
    #sc_ack_msg{id=MsgId ,code=Code, info=Info}.
timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

