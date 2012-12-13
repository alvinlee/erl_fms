%%%-------------------------------------------------------------------
%%% File    : ria_proxy.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created :  7 Sep 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(ria_proxy).

-behaviour(gen_fsm).

%% API
-export([start/1]).

%% gen_fsm callbacks
-export([init/1, sock_init/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
          sock_in,
          sock_out,
          dec_in,
          dec_out
         }).

-define(SERVER,?MODULE).

-include("logger.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start(SockIn) ->
    Ret = {ok,Pid} = gen_fsm:start(?MODULE, [], []),
    ok = gen_tcp:controlling_process(SockIn,Pid),
    gen_fsm:send_event(Pid,{sock_ready,SockIn}),
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
    {ok, sock_init, #state{}}.

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
sock_init({sock_ready,SockIn}, State) ->
    Opts = [{active,false},{send_timeout,5000},
            {keepalive,true},{packet,raw},binary],
    {ok,SockOut} = gen_tcp:connect({0,0,0,0},1936,Opts),

    {ok, HSInReq} = gen_tcp:recv(SockIn,1537),
    ok = gen_tcp:send(SockOut,HSInReq),
    
    {ok, HSOutReq} = gen_tcp:recv(SockOut,1537),
    {ok, HSOutResp} = gen_tcp:recv(SockOut,1536),
    ok = gen_tcp:send(SockIn,[HSOutReq,HSOutResp]),

    {ok, HSInResp} = gen_tcp:recv(SockIn,1536),
    ok = gen_tcp:send(SockOut,HSInResp),

    inet:setopts(SockIn,[{active,true}]),
    inet:setopts(SockOut,[{active,true}]),

    {next_state, ready, State#state{sock_in=SockIn,
                                    sock_out=SockOut,
                                    dec_in=ria_rtmp:new(),
                                    dec_out=ria_rtmp:new()
                                   }}.

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
handle_info({tcp, Socket, Data}, StateName, #state{sock_in=Socket}=State) ->
    {Results,Dec} = (State#state.dec_in):decode(Data),
    ?INFO("~p ---> ~P~n",[size(Data),Results,20]),
    ok = gen_tcp:send(State#state.sock_out,Data),
    {next_state, StateName, State#state{dec_in=Dec}};

handle_info({tcp, Socket, Data}, StateName, #state{sock_out=Socket}=State) ->
    {Results,Dec} = (State#state.dec_out):decode(Data),
    ?INFO("~p <--- ~P~n",[size(Data),Results,20]),
    ok = gen_tcp:send(State#state.sock_in,Data),
    {next_state, StateName, State#state{dec_out=Dec}};

handle_info({tcp_closed, Socket}, _StateName,State) ->
    ?WARN("~p socket closed.~n",[Socket]),
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, _StateName, State) ->
    ?WARN("~p socket error ~p.~n",[Socket,Reason]),
    {stop, {tcp_error, Reason}, State};

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
