%%%-------------------------------------------------------------------
%%% File    : ria_flv.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 30 Sep 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_flv).

%% API
-export([decode_tag/1,
         decode_tag/2]).

-include("ria_def.hrl").
-include("logger.hrl").

%%====================================================================
%% API
%%====================================================================
decode_tag(<<Code:?UB8,Len:?UB24,Stamp1:?UB24,Stamp2:?UB8,Stream:?UB24,
            Body:Len/binary,Size:?UB32,Rest/binary>>) ->
    if Size =/= (Len + 11) -> ?ERROR("~p ~p~n",[Size,Len]);
       true -> ignore end,
    Type = ?decode(tag,Code),
    Data = decode_tag(Type,Body),
    Cur=#rtmp{stamp={Stamp1,Stamp2},len=Len,type=Type,
              stream=Stream,next=Size,body=Data},
    {Cur,Rest}.

%%%-------------------------------------------------------------------
decode_tag('AudioData',Body) -> Body;

decode_tag('VideoData',<<Type:?UB4,Codec:?UB4,Data/binary>>) -> 
    {?decode(frame,Type),?decode(codec,Codec),Data};

decode_tag('ScriptData',Body) -> ria_amf0:decode(Body);

decode_tag(_,Body) -> Body.
    
%%====================================================================
%% Internal functions
%%====================================================================
?DefCodeMap(tag,'AudioData',  8);
?DefCodeMap(tag,'VideoData',  9);
?DefCodeMap(tag,'ScriptData', 18);
?DefCodeMap(tag,Unkown,Unkown);

?DefCodeMap(frame,'Key',        1);
?DefCodeMap(frame,'Inter',      2);
?DefCodeMap(frame,'Disposable', 3);
?DefCodeMap(frame,'ServerKey',  4);
?DefCodeMap(frame,'InfoCmd',    5);
?DefCodeMap(frame,Unkown,Unkown);

?DefCodeMap(codec,'JPEG',    1);
?DefCodeMap(codec,'H263',    2);
?DefCodeMap(codec,'Screen',  3);
?DefCodeMap(codec,'VP6',     4);
?DefCodeMap(codec,'VP6A',    5);
?DefCodeMap(codec,'Screen2', 6);
?DefCodeMap(codec,'AVC',     7);
?DefCodeMap(codec,Unkown,Unkown).
