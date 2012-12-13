%%%-------------------------------------------------------------------
%%% File    : ria_rtmp.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 12 Sep 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_rtmp).

%% API
-export([new/0,
         decode/1,
         decode/2,
         get_result/1
        ]).

-record(?MODULE,{
           chunk=128,
           dict=dict:new(),
           state=header,
           buf= <<>>,
           size=0,
           cur,
           ret=[]
          }).

-include("ria_def.hrl").
-include("logger.hrl").

%%%-------------------------------------------------------------------
?DefCodeMap(rtmp,'ChunkSize',        16#01);
?DefCodeMap(rtmp,'BytesRead',        16#03);
?DefCodeMap(rtmp,'Ping',             16#04);
?DefCodeMap(rtmp,'BW_S2C',           16#05);
?DefCodeMap(rtmp,'BW_C2S',           16#06);
?DefCodeMap(rtmp,'AudioData',        16#08);
?DefCodeMap(rtmp,'VideoData',        16#09);
?DefCodeMap(rtmp,'FlexStream',       16#0F);
?DefCodeMap(rtmp,'FlexSharedObject', 16#10);
?DefCodeMap(rtmp,'Call',             16#11);
?DefCodeMap(rtmp,'Notify',           16#12);
?DefCodeMap(rtmp,'SharedObject',     16#13);
?DefCodeMap(rtmp,'Invoke',           16#14);
?DefCodeMap(rtmp,'FlvTags',          16#16);
?DefCodeMap(rtmp,Unkown,Unkown);

?DefCodeMap(ping,'Clear',    16#00);
?DefCodeMap(ping,'Play',     16#01);
?DefCodeMap(ping,'Buffer',   16#03);
?DefCodeMap(ping,'Reset',    16#04);
?DefCodeMap(ping,'Request',  16#06);
?DefCodeMap(ping,'Response', 16#07);
?DefCodeMap(ping,Unkown,Unkown).

%%====================================================================
%% API
%%====================================================================
new() -> #?MODULE{}.

decode(Bin,#?MODULE{buf=Buf,size=Size}=State) ->
    CurBuf = <<Buf/binary,Bin/binary>>,
    CurSize = Size+size(Bin),
    CurState = State#?MODULE{buf=CurBuf,size=CurSize},
    CurState:decode().

decode(#?MODULE{size=0}=State)       -> State:get_result();
decode(#?MODULE{state=header}=State) -> decode_header(State);
decode(#?MODULE{state=body}=State)   -> decode_body(State).

%%====================================================================
%% Internal functions
%%====================================================================
get_result(State) -> 
    {lists:reverse(State#?MODULE.ret),State#?MODULE{ret=[]}}.

%%%-------------------------------------------------------------------
decode_header(State) when State#?MODULE.size < 1 -> State:get_result();
decode_header(#?MODULE{buf= <<2#11:2,Id:6,Buf/binary>>,
                       size=Size,dict=Dict}=State) ->
    Cur = Dict:fetch(Id),
    CurState = State#?MODULE{state=body,buf=Buf,size=Size-1,cur=Cur},
    CurState:decode();

%%%-------------------------------------------------------------------
decode_header(State) when State#?MODULE.size < 4 -> State:get_result();
decode_header(#?MODULE{buf= <<2#10:2,Id:6,Stamp:?SB24,Buf/binary>>,
                       size=Size,dict=Dict}=State) ->
    Cur = Dict:fetch(Id),
    Cur2 = Cur#rtmp{stamp=Stamp},
    CurDict = Dict:store(Id,Cur2),
    CurState = State#?MODULE{state=body,buf=Buf,size=Size-4,cur=Cur2,dict=CurDict},
    CurState:decode();

%%%-------------------------------------------------------------------
decode_header(State) when State#?MODULE.size < 8 -> State:get_result();
decode_header(#?MODULE{buf= <<2#01:2,Id:6,Stamp:?SB24,Len:?UB24,
                             Type:?UB8,Buf/binary>>,
                       size=Size,dict=Dict}=State) ->
    Cur = Dict:fetch(Id),
    Cur2 = Cur#rtmp{stamp=Stamp,len=Len,next=Len,type=?decode(rtmp,Type)},
    CurDict = Dict:store(Id,Cur2),
    CurState = State#?MODULE{state=body,buf=Buf,size=Size-8,cur=Cur2,dict=CurDict},
    CurState:decode();

%%%-------------------------------------------------------------------
decode_header(State) when State#?MODULE.size < 12 -> State:get_result();
decode_header(#?MODULE{buf= <<2#00:2,Id:6,Stamp:?SB24,Len:?UB24,
                             Type:?UB8,Stream:?UL32,Buf/binary>>,
                       size=Size,dict=Dict}=State) ->
    Cur = #rtmp{id=Id,stamp=Stamp,len=Len,next=Len,stream=Stream,
                type=?decode(rtmp,Type),body=[]},
    CurDict = Dict:store(Id,Cur),
    CurState = State#?MODULE{state=body,buf=Buf,size=Size-12,cur=Cur,dict=CurDict},
    CurState:decode().

%%%-------------------------------------------------------------------
decode_body(#?MODULE{cur=#rtmp{next=Next},chunk=Chunk,size=Size}=State)
  when (Next=<Chunk) and (Next=<Size) -> 
    #?MODULE{cur=Cur,dict=Dict,ret=Ret,buf=Buf}=State,
    #rtmp{id=Id,len=Len,body=Last}=Cur,
    <<Body:Next/binary,Rest/binary>> = Buf,
    Cur2 = Cur#rtmp{next=Len,body=[]},
    CurDict = Dict:store(Id,Cur2),
    Cur3 = decode_body(Cur2,iolist_to_binary(lists:reverse([Body|Last]))),
    CurChunk = update_chunk(Cur3,Chunk),
    CurState = State#?MODULE{state=header,chunk=CurChunk,buf=Rest,dict=CurDict,
                             size=Size-Next,ret=[Cur3|Ret]},
    CurState:decode();

decode_body(#?MODULE{cur=#rtmp{next=Next},chunk=Chunk,size=Size}=State)
  when (Next>Chunk) and (Size>=Chunk) -> 
    #?MODULE{cur=Cur,dict=Dict,ret=Ret,buf=Buf}=State,
    #rtmp{id=Id,len=Len,body=Last}=Cur,
    <<Body:Chunk/binary,Rest/binary>> = Buf,
    CurDict = Dict:store(Id,Cur#rtmp{next=Next-Chunk,body=[Body|Last]}),
    CurState = State#?MODULE{state=header,buf=Rest,dict=CurDict,size=Size-Chunk},
    CurState:decode();

decode_body(State) -> State:get_result().

%%%-------------------------------------------------------------------
decode_body(#rtmp{type='ChunkSize'}=Cur,<<ChunkSize:?UB32>>) ->
    Cur#rtmp{body=ChunkSize};

decode_body(#rtmp{type='Ping'}=Cur,<<Type:?UB16,Val:?UB32>>) ->
    Cur#rtmp{body={?decode(ping,Type),Val}};

decode_body(#rtmp{type='Ping'}=Cur,<<Type:?UB16,Val1:?UB32,Val2:?UB32>>) ->
    Cur#rtmp{body={?decode(ping,Type),Val1,Val2}};

decode_body(#rtmp{type='BW_S2C'}=Cur,<<Val:?UB32>>) ->
    Cur#rtmp{body=Val};

decode_body(#rtmp{type='BW_C2S'}=Cur,<<Val1:?UB32,Val2:?UB8>>) ->
    Cur#rtmp{body={Val1,Val2}};

decode_body(#rtmp{type='AudioData'=Type}=Cur,Body) ->
    Cur#rtmp{body=ria_flv:decode_tag(Type,Body)};

decode_body(#rtmp{type='VideoData'=Type}=Cur,Body) ->
    Cur#rtmp{body=ria_flv:decode_tag(Type,Body)};

decode_body(#rtmp{type='Call'}=Cur,<<0:?UB8,Body/binary>>) ->
    Cur#rtmp{body=decode_func(Body)};

decode_body(#rtmp{type='Notify'}=Cur,Body) ->
    [Fun|Parms] = ria_amf0:decode(Body),
    Cur#rtmp{body={list_to_atom(Fun),Parms}};

decode_body(#rtmp{type='Invoke'}=Cur,Body) ->
    Cur#rtmp{body=decode_func(Body)};

decode_body(#rtmp{type='FlvTags'}=Cur,Body) ->
    Cur#rtmp{body=ria_utils:decode_loop(Body,fun ria_flv:decode_tag/1)};

decode_body(Header,Body) -> {Header,Body}.

%%%-------------------------------------------------------------------
update_chunk(#rtmp{type='ChunkSize'}=Cur,_) -> Cur#rtmp.body;
update_chunk(_,Last) -> Last.

%%%-------------------------------------------------------------------
decode_func(Body) ->
    [Fun,Id,Info|Parms] = ria_amf0:decode(Body),
    #rtmp_func{id=Id,func=list_to_atom(Fun),info=Info,parms=Parms}.
