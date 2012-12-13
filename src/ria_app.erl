%%%-------------------------------------------------------------------
%%% File    : ria_app.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 13 Aug 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    ria_drv:open("/home/alvin/src/erlria/build/c_src/.libs"),

    Modules = [{tcp_acceptor,[p2p_acceptor,{
                                {0,0,0,0},1935,[],10,
                                {ria_proxy,start,[]}}]}],
    
    %% Modules = [{tcp_acceptor,[p2p_acceptor,{
    %%                             {0,0,0,0},1935,[],10,
    %%                             {ria_nc,start,[]}}]}],

    ria_sup:start_link(Modules).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ria_drv:close(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

