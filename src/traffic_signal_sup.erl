%%%-------------------------------------------------------------------
%% @doc traffic_signal top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(traffic_signal_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 0, 1}, child_specs() }}.

%%====================================================================
%% Internal functions
%%====================================================================

child_specs() ->
    {ok, Pins} =  application:get_env(pins),
    [child_spec_for(PinName, Pin) || {PinName, Pin} <- Pins].

child_spec_for(PinName, Pin) ->
    #{
        id => PinName,
        start => {traffic_signal_light, start_link, [PinName, Pin]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [traffic_signal_light]
    }.

