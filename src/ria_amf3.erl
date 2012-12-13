%%%-------------------------------------------------------------------
%%% File    : ria_amf3.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 15 Sep 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_amf3).

%% API
-export([decode/1]).

-include("ria_def.hrl").
-include("logger.hrl").

%%====================================================================
%% API
%%====================================================================
decode(Bin) -> ria_utils:decode_loop(Bin,fun decode_bin/1).

%%====================================================================
%% Internal functions
%%====================================================================
?DefCodeMap(amf3,'Undefined', 16#00);
?DefCodeMap(amf3,'Null',      16#01);
?DefCodeMap(amf3,'False',     16#02);
?DefCodeMap(amf3,'True',      16#03);
?DefCodeMap(amf3,'Integer',   16#04);
?DefCodeMap(amf3,'Double',    16#05);
?DefCodeMap(amf3,'String',    16#06);
?DefCodeMap(amf3,'XMLDoc',    16#07);
?DefCodeMap(amf3,'Date',      16#08);
?DefCodeMap(amf3,'Array',     16#09);
?DefCodeMap(amf3,'Object',    16#0A);
?DefCodeMap(amf3,'XML',       16#0B);
?DefCodeMap(amf3,'ByteArray', 16#0C);
?DefCodeMap(amf3,Unkown,Unkown).

%%%-------------------------------------------------------------------
decode_bin(<<Type:?UB8,Bin/binary>>) -> decode_type(?decode(amf3,Type),Bin).

decode_type('Undefined',Bin)                                      -> {undefined,Bin};
decode_type('Null',Bin)                                           -> {{},Bin};
decode_type('False',Bin)                                          -> {false,Bin};
decode_type('True',Bin)                                           -> {true,Bin};
decode_type('Integer',Bin)                                        -> decode_u29(Bin);
decode_type('Double',<<Num:?DB64,Bin/binary>>)                    -> {Num,Bin};
decode_type('String',Bin)                                         -> decode_str(Bin);
%% decode_type('XMLDoc',Bin)                                         -> decode_type('LongString',Bin);
%% decode_type('Date',<<Date:?DB64,TZ:?SB16,Bin/binary>>)            -> {{date,Date,TZ},Bin};
%% decode_type('Array',Bin)                                      -> {array,Bin};
%% decode_type('Object',Bin)                                         -> decode_obj(Bin,[]);
%% decode_type('XML',Bin)                                         -> decode_type('LongString',Bin);
decode_type('ByteArray',Bin)                 -> decode_bytes(Bin).

%%%-------------------------------------------------------------------
decode_u29(<<0:1,Val:?UB(7), Bin/binary>>) -> {Val,Bin};
decode_u29(<<1:1,Bits1:7, 0:1,Bits2:7, Bin/binary>>) ->
    <<Val:?UB(14)>> = <<Bits1/bits,Bits2/bits>>,{Val,Bin};
decode_u29(<<1:1,Bits1:7, 1:1,Bits2:7, 0:1,Bits3:7, Bin/binary>>) ->
    <<Val:?UB(21)>> = <<Bits1/bits,Bits2/bits,Bits3/bits>>,
    {Val,Bin};
decode_u29(<<1:1,Bits1:7, 1:1,Bits2:7, 1:1,Bits3:7, Bits4:8,Bin/binary>>) ->
    <<Val:?UB(29)>> = <<Bits1/bits,Bits2/bits,Bits3/bits,Bits4/bits>>,
    {Val,Bin}.

%%%-------------------------------------------------------------------
decode_ref(Bin,Fun) ->
    {U29,CurBin} = decode_u29(Bin),
    <<Flag:1,Size:?SB(28)>> = <<U29:?UB(29)>>,
    decode_ref(Flag,Size,CurBin,Fun).

decode_ref(0,Ref,Bin,_Fun) -> {{ref,Ref},Bin};
decode_ref(1,Val,Bin,Fun) -> Fun(Val,Bin).

%%%-------------------------------------------------------------------
decode_str(Bin) -> decode_ref(Bin,fun decode_str/2).

decode_str(Size,Bin) -> ria_utils:decode_count(Size,Bin,fun decode_utf8/1).

decode_utf8(<<0:1,_:7, _/binary>> = Bin) -> decode_byte(Bin);

decode_utf8(<<2#110:3,_:5, 2#10:2,_:6, _/binary>> = Bin) -> 
    ria_utils:decode_count(2,Bin,fun decode_byte/1);
    
decode_utf8(<<2#1110:4,_:4, 2#10:2,_:6, 2#10:2,_:6, _/binary>> = Bin) -> 
    ria_utils:decode_count(3,Bin,fun decode_byte/1);

decode_utf8(<<2#11110:5,_:3, 2#10:2,_:6, 2#10:2,_:6, 2#10:2,_:6, _/binary>> = Bin) -> 
    ria_utils:decode_count(4,Bin,fun decode_byte/1).

decode_byte(<<Val:?UB8,Bin/binary>>) -> {Val,Bin}.

%%%-------------------------------------------------------------------
    


%%%-------------------------------------------------------------------
decode_bytes(Bin) -> decode_ref(Bin,fun decode_bytes/2).
    
decode_bytes(Size,Bin) -> ria_utils:decode_count(Size,Bin,fun decode_byte/1).
