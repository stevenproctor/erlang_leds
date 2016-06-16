-module(leds).

-export([blinky_blink/3]).

blinky_blink(Leds, Cycles, Intervals) ->
    lists:foreach( fun(Led) -> traffic_signal_light:blink(Led, Cycles, pick_interval(Intervals)) end, Leds).


pick_interval(Intervals) ->
    OnInterval = pick_random(Intervals),
    OffInterval = pick_random(Intervals),
    #{on => OnInterval, off => OffInterval}.

pick_random(List) ->
    Length = length(List),
    lists:nth(crypto:rand_uniform(1, Length), List).
