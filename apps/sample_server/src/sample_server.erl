%%%-------------------------------------------------------------------
%%% @author Roland Karlsson <roland@dundee.local>
%%% @copyright (C) 2014, Roland Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2014 by Roland Karlsson <roland@dundee.local>
%%%-------------------------------------------------------------------
-module(sample_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([load_agent/1, start_agent/0, start_sampling/0, stop_agent/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {agent}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

load_agent(AgentNode) ->
    gen_server:call(?SERVER, {load_agent,AgentNode}).

start_agent() ->
    gen_server:call(?SERVER, start_agent).

start_sampling() ->
    gen_server:call(?SERVER, start_sampling).

stop_agent() ->
    gen_server:call(?SERVER, stop_agent).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({load_agent, AgentNode}, _From, State) ->
    Reply = do_load_agent(AgentNode),
    {reply, Reply, State#state{agent = AgentNode}};

handle_call(start_agent, _From, State=#state{agent=AgentNode}) ->
    Reply = do_start_agent(AgentNode),
    {reply, Reply, State};

handle_call(start_sampling, _From, State=#state{agent=AgentNode}) ->
    Reply = do_start_sampling(AgentNode),
    {reply, Reply, State};

handle_call(stop_agent, _From, State=#state{agent=AgentNode}) ->
    Reply = do_stop_agent(AgentNode),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    erlang:display(Info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Local

do_load_agent(Node) ->
    {Module, Code, File} = code:get_object_code(sample_agent),
    rpc:call(Node,
             code, load_binary, [Module, File, Code],
             10000).

do_start_agent(Node) ->
    rpc:call(Node,
             sample_agent, start_link, [],
             10000).

do_start_sampling(Node) ->
    rpc:call(Node,
             sample_agent, start_sampling, [self(), 100],
             10000).

do_stop_agent(Node) ->
    rpc:cast(Node,
             sample_agent, stop, []).
