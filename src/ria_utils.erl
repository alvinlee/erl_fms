%%%-------------------------------------------------------------------
%%% File    : ria_utils.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 30 Sep 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_utils).

%% API
-export([decode_loop/2,
         decode_until/3,
         decode_count/3]).

%%====================================================================
%% API
%%====================================================================
decode_loop(Bin,Fun) -> decode_loop_rec(Bin,Fun,[]).
decode_loop_rec(<<>>,Fun,Last) -> lists:reverse(Last);
decode_loop_rec(Bin,Fun,Last) ->
    {Val,CurBin} = Fun(Bin),
    decode_loop_rec(CurBin,Fun,[Val|Last]).

decode_until(End,Bin,Fun) -> decode_until_rec(End,Bin,Fun,[]).
decode_until_rec(End,Bin,Fun,Last) ->
    case Fun(Bin) of
        {End,CurBin} -> {lists:reverse(Last),CurBin};
        {Val,CurBin} -> decode_until_rec(End,CurBin,Fun,[Val|Last]) 
    end.

decode_count(Count,Bin,Fun) -> decode_count_rec(Count,Bin,Fun,[]).
decode_count_rec(0,Bin,_,Last) -> {lists:reverse(Last),Bin};
decode_count_rec(Count,Bin,Fun,Last) ->
    {Val,CurBin} = Fun(Bin),
    decode_count_rec(Count-1,CurBin,Fun,[Val|Last]).

%%====================================================================
%% Internal functions
%%====================================================================
