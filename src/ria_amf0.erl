%%%-------------------------------------------------------------------
%%% File    : ria_amf0.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 15 Sep 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_amf0).

%% API
-export([decode/1,decode_obj_item/1]).

-include("ria_def.hrl").
-include("logger.hrl").

%%====================================================================
%% API
%%====================================================================
decode(Bin) -> ria_utils:decode_loop(Bin,fun decode_bin/1).

%%====================================================================
%% Internal functions
%%====================================================================
?DefCodeMap(amf0,'Number',      16#00);
?DefCodeMap(amf0,'Boolean',     16#01);
?DefCodeMap(amf0,'String',      16#02);
?DefCodeMap(amf0,'Object',      16#03);
?DefCodeMap(amf0,'MovieClip',   16#04);
?DefCodeMap(amf0,'Null',        16#05);
?DefCodeMap(amf0,'Undefined',   16#06);
?DefCodeMap(amf0,'Reference',   16#07);
?DefCodeMap(amf0,'ECMAArray',   16#08);
?DefCodeMap(amf0,'ObjectEnd',   16#09);
?DefCodeMap(amf0,'StrictArray', 16#0A);
?DefCodeMap(amf0,'Date',        16#0B);
?DefCodeMap(amf0,'LongString',  16#0C);
?DefCodeMap(amf0,'Unsupported', 16#0D);
?DefCodeMap(amf0,'Recordset',   16#0E);
?DefCodeMap(amf0,'XMLDoc',      16#0F);
?DefCodeMap(amf0,'TypedObject', 16#10);
?DefCodeMap(amf0,'AMF3',        16#11);
?DefCodeMap(amf0,Unkown,Unkown).

%%%-------------------------------------------------------------------
decode_bin(<<Type:?UB8,Bin/binary>>) -> decode_type(?decode(amf0,Type),Bin).

decode_type('Number',<<Num:?DB64,Bin/binary>>)                    -> {Num,Bin};
decode_type('Boolean',<<0:?UB8,Bin/binary>>)                      -> {false,Bin};
decode_type('Boolean',<<_:?UB8,Bin/binary>>)                      -> {true,Bin};
decode_type('String',<<Len:?UB16,Str:Len/binary,Bin/binary>>)     -> {binary_to_list(Str),Bin};
decode_type('Object',Bin)                                         -> decode_obj(Bin);
decode_type('Null',Bin)                                           -> {null,Bin};
decode_type('Undefined',Bin)                                      -> {undefined,Bin};
decode_type('Reference',<<Ref:?UB16,Bin/binary>>)                 -> {{ref,Ref},Bin};
decode_type('ECMAArray',Bin)                                      -> decode_array(Bin);
decode_type('ObjectEnd',Bin)                                      -> {obj_end,Bin};
decode_type('StrictArray',Bin)                                    -> decode_sarray(Bin);
decode_type('Date',<<Date:?DB64,TZ:?SB16,Bin/binary>>)            -> {{date,Date,TZ},Bin};
decode_type('LongString',<<Len:?UB32,Str:Len/binary,Bin/binary>>) -> {binary_to_list(Str),Bin};
decode_type('XMLDoc',Bin)                                         -> decode_type('LongString',Bin);
decode_type('TypedObject',Bin)                                    -> {obj,Bin};
decode_type('AMF3',Bin)                                           -> {obj,Bin}.

%%%-------------------------------------------------------------------
decode_obj(Bin) -> 
    {Obj,Rest} = ria_utils:decode_until({'',obj_end},Bin,fun decode_obj_item/1),
    {{obj,Obj},Rest}.
decode_obj_item(Bin) ->
    {Key,Bin1} = decode_type('String',Bin),
    {Val,Bin2} = decode_bin(Bin1),
    {{list_to_atom(Key),Val},Bin2}.

%%%-------------------------------------------------------------------
decode_array(<<Count:?UB32,Bin/binary>>) ->
    {Val,CurBin} = decode_obj(Bin),
    {{array,Count,Val},CurBin}.

%%%-------------------------------------------------------------------
decode_sarray(<<Count:?UB32,Bin/binary>>) ->
    {Val,CurBin} = ria_utils:decode_count(Count,Bin,fun decode_bin/1),
    {{sarray,Count,Val},CurBin}.
