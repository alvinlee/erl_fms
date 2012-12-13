%%%-------------------------------------------------------------------
%%% File    : ria_drv.erl
%%% Author  : Alvin.Lee <alvin.msg@gmail.com>
%%% Description : 
%%%
%%% Created : 17 Nov 2008 by Alvin.Lee <alvin.msg@gmail.com>
%%%-------------------------------------------------------------------
-module(ria_drv).

%% API
-export([open/0,
         open/1,
         close/0,
         handshake/1
        ]).

-define(DRV_NAME, erlria_drv).

-define(HANDSHAKE, 1).

%%====================================================================
%% API
%%====================================================================
open() ->
    {ok, App} = application:get_application(),
    Path = code:priv_dir(App),
    open(Path).

open(Path) ->
    case erl_ddll:load_driver(Path, ?DRV_NAME) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, ErrorDesc} -> exit({error, erl_ddll:format_error(ErrorDesc)})
    end,
    
    Port = open_port({spawn, ?DRV_NAME}, [binary]),
    register(?DRV_NAME,Port).

close() ->
    port_close(?DRV_NAME).

handshake(Req) ->
    port_control(?DRV_NAME, ?HANDSHAKE, Req).

%%====================================================================
%% Internal functions
%%====================================================================
