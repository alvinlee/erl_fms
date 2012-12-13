%%%-------------------------------------------------------------------
%%% File    : ria_sup.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 13 Aug 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Modules) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Modules).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init(Modules) ->
    {ok,{{one_for_one,1,1}, lists:map(fun make_spec/1,Modules)}}.

%%====================================================================
%% Internal functions
%%====================================================================
make_spec({Module,Args}) ->
    {Module,{Module,start_link,Args},permanent,2000,worker,[Module]}.
