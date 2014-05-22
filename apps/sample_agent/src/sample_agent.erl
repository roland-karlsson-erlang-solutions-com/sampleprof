%%%-------------------------------------------------------------------
%%% @author 
%%% @copyright (C)
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2014 by
%%%-------------------------------------------------------------------
-module(sample_agent).

-behaviour(gen_server).

%% API
-export([start_link/0, start_sampling/2, stop_sampling/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { running_procs,
		 receiver,
		 interval,
		 sampling_enabled,
		 timer_ref}).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts sampling with the given interval that sends stack traces 
%% back to the given receiver.
%%
%% @spec start_sampling() -> ok
%% @end
%%--------------------------------------------------------------------
start_sampling(Receiver, Interval) ->
    gen_server:cast(?SERVER, {start_sampling, Receiver, Interval}).

%%--------------------------------------------------------------------
%% @doc
%% Stops sampling if sampling is started. Has no effect if sampling is
%% not started.
%%
%% @spec stop_sampling() -> ok
%% @end
%%--------------------------------------------------------------------
stop_sampling() ->
    gen_server:cast(?SERVER, stop_sampling).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{ }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



handle_cast({start_sampling, Receiver, Interval},
	    #state{ sampling_enabled = Enabled, timer_ref = OldTimerRef }) ->
    if
	Enabled =:= true ->
	    do_stop_sampling(OldTimerRef);
	true ->
	    ok
    end,
    TimerRef = do_start_sampling(Interval),
    {noreply, #state{ running_procs = [],
		      receiver = Receiver,
		      interval = Interval,
		      sampling_enabled = true,
		      timer_ref = TimerRef }};

handle_cast(stop_sampling, #state{ timer_ref = TimerRef }) ->
    erlang:display(stopping),
    do_stop_sampling(TimerRef),
    {noreply, #state{ sampling_enabled = false} };

handle_cast(stop, State) ->
    {stop, normal, State}.



handle_info({trace, Pid, in, _Funct},
	    State = #state{ running_procs = Procs }) ->
    {noreply, State#state{ running_procs = [ Pid | Procs] }};

handle_info({trace, Pid, out, _Funct},
	    State = #state{ running_procs = Procs }) ->
    RunningProcs = [ P || P <- Procs, P =/= Pid ],
    {noreply, State#state{ running_procs = RunningProcs }};

handle_info(perform_sampling,
	    State = #state{ running_procs = RunningProcs,
			    receiver = Receiver }) ->
    do_perform_sampling(RunningProcs, Receiver),
    {noreply, State};

handle_info(_Other, _State) ->
    error("Unexpected message").



terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_start_sampling(Interval) ->
    start_trace_running(),
    TimerRef = start_periodic_sampling(Interval),
    TimerRef.

do_stop_sampling(TimerRef) ->
    stop_trace_running(),
    timer:cancel(TimerRef).

do_perform_sampling(RunningProcs, Receiver) ->
    StackTraces = [ proplists:lookup(current_stacktrace, PI) ||
		      PI <- [ get_process_info(P) || P <- RunningProcs ],
		      PI =/= undefined,
		      proplists:lookup(status, PI) =:= running ],
    Receiver ! {stacktrace_sample, self(), StackTraces}.

start_trace_running() ->
    erlang:trace(all, true, [running]).

stop_trace_running() ->
    erlang:trace(all, false, [running]).

start_periodic_sampling(Interval) ->
    {ok, TimerRef} = timer:send_interval(Interval, perform_sampling),
    TimerRef.

get_process_info(Pid) ->
    erlang:process_info(Pid, [status, current_stacktrace]).
