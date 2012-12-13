%%%-------------------------------------------------------------------
%%% File    : ria_nc.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 10 Nov 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_nc).

-behaviour(gen_fsm).

%% API
-export([start/1]).

%% gen_fsm callbacks
-export([init/1, 
         sock_wait/2,
         handshake/2,
         state_name/2, 
         state_name/3, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

-record(state, {sock,rtmp}).

-include("logger.hrl").
-include("ria_def.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start({ok,Sock}) ->
    Ret = {ok,Pid} = gen_fsm:start(?MODULE, [], []),
    ok = gen_tcp:controlling_process(Sock,Pid),
    gen_fsm:send_event(Pid,{sock_ready,Sock}),
    Ret.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([]) ->
    {ok, sock_wait, #state{}}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
sock_wait({sock_ready,Sock}, State) ->
    handshake(handshake, State#state{sock=Sock}).

handshake(handshake, #state{sock=Sock}=State) ->
    {ok, <<3,Req/binary>>} = gen_tcp:recv(Sock,?HANDSHAKE_SIZE+1,?RECV_TIMEOUT),
    Resp = ria_drv:handshake(Req),
    ok = gen_tcp:send(Sock,[3,?HANDSHAKE,Resp]),
    {ok, _} = gen_tcp:recv(Sock,?HANDSHAKE_SIZE,?RECV_TIMEOUT),
    inet:setopts(Sock,[{active,once},{keepalive,true},{send_timeout,?SEND_TIMEOUT}]),
    {next_state, connect, State#state{rtmp=ria_rtmp:new()}}.

connect(#rtmp{}, State) ->
    {next_state, state_name, State}.

active(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, StateName, State) ->
    {Reqs,RTMP} = (State#state.rtmp):decode(Data),
    ?INFO("---> ~p ~P ~n",[size(Data),Reqs,30]),
    InitRet = {next_state,StateName,State#state{rtmp=RTMP}},
    CurRet = lists:foldl(fun process_request/2,InitRet,Reqs),
    inet:setopts(Socket,[{active,once}]),CurRet;

handle_info({tcp_closed, _Socket}, _StateName, State) ->
    ?WARN("market socket closed.~n",[]),
    {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, _StateName, State) ->
    ?WARN("peer socket error ~p.~n",[Reason]),
    {stop, normal, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
process_request(Req,{next_state,StateName,State}) ->
    apply(?MODULE,StateName,[Req,State]);
process_request(_,Ret) -> Ret.
