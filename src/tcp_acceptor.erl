%%%-------------------------------------------------------------------
%%% File    : tcp_acceptor.erl
%%% Author  : alvin <>
%%% Description : 
%%%
%%% Created : 18 Jul 2008 by alvin <>
%%%-------------------------------------------------------------------
-module(tcp_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {accept_args}).
-include("logger.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name,Args) ->
    gen_server:start_link({local,Name},?MODULE,Args,[]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init({Ip,Port,Opts,Count,OnAccept}) ->
	process_flag(trap_exit, true),

    %% listen
	ListenOpts = [{ip, Ip},
                  {active, false},
                  {backlog, 30},
                  {reuseaddr, true},
                  binary],
    
    case gen_tcp:listen(Port, Opts ++ ListenOpts) of
        {ok, LSock} ->
            %% spwan accept process
            AcceptArgs = {LSock,OnAccept},
            lists:foreach(fun swpan_accept_process/1, 
                          lists:duplicate(Count,AcceptArgs)),
            {ok, #state{accept_args=AcceptArgs}};
        {error, Reason} -> 
            ?ERROR("listen error ~p~n",[Reason]),
            {stop,normal}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', _From, _Reason}, State) ->
    swpan_accept_process(State#state.accept_args),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
swpan_accept_process(AcceptArgs) ->
    proc_lib:spawn_link(fun()->accept_loop(AcceptArgs) end).

accept_loop({LSock,{M, F, A}}=AcceptArgs) ->
    SysLoad = cpu_sup:util(),
    case gen_tcp:accept(LSock) of
        {ok,Sock} when SysLoad < 85 -> apply(M, F, A++[Sock]);
        {ok,Sock} -> 
            ?WARN("load too high,close new connection~n",[]),
            gen_tcp:close(Sock);
        {error, Reason} -> ?WARN("accept error ~p~n",[Reason]) 
    end,

    accept_loop(AcceptArgs).
