%%%-------------------------------------------------------------------
%%% @author proctor
%%% @copyright (C) 2016, proctor
%%% @doc
%%%
%%% @end
%%% Created : 2016-05-03 23:36:59.243210
%%%-------------------------------------------------------------------
-module(traffic_signal_light).

-behaviour(gen_server).

%% API
-export([start_link/2,
         on/1,
         off/1,
         blink/2,
         blink/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {gpioPid}).

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
start_link(Color, Pin) ->
        gen_server:start_link({global, {?SERVER, Color}}, ?MODULE, [Pin], []).

on(Color) ->
        gen_server:cast({global, {?SERVER, Color}}, on).

off(Color) ->
        gen_server:cast({global, {?SERVER, Color}}, off).

blink(Color, Cycles) ->
        blink(Color, Cycles, #{on => 100, off => 100}).

blink(Color, Cycles, Settings) ->
        gen_server:cast({global, {?SERVER, Color}}, {blink, Cycles, Settings}).

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
init([Pin]) ->
        {ok, Pid} = gpio:start_link(Pin, output),
        {ok, #state{gpioPid=Pid}}.

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
handle_cast(on, State=#state{gpioPid=GpioPid}) ->
        turn_on(GpioPid),
        {noreply, State};
handle_cast(off, State=#state{gpioPid=GpioPid}) ->
        turn_off(GpioPid),
        {noreply, State};
handle_cast({blink, Cycles, Timing}, State=#state{gpioPid=GpioPid}) ->
        do_blink(GpioPid, Cycles, Timing),
        {noreply, State};
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State=#state{gpioPid=GpioPid}) ->
        turn_off(GpioPid),
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


turn_on(GpioPid) ->
    gpio:write(GpioPid, 1).

turn_off(GpioPid) ->
    gpio:write(GpioPid, 0).

do_blink(_GpioPid, 0, _Timing) ->
    ok;
do_blink(GpioPid, Cycles, Timing=#{on := OnTime, off := OffTime}) ->
    turn_on(GpioPid),
    timer:sleep(OnTime),
    turn_off(GpioPid),
    timer:sleep(OffTime),
    do_blink(GpioPid, Cycles-1, Timing).
