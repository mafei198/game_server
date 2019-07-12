-module(event_center).

-export([subscribe/2,
         publish/1,
         global_publish/2 %% 服务器事件
        ]).

-include("include/gproc_macros.hrl").

subscribe(EventName, Callback) ->
    Key = {event_listeners, EventName},
    case get(Key) of
        undefined -> put(Key, [#{callback => Callback}]);
        Listeners -> put(Key, [#{callback => Callback}|Listeners])
    end.

%% Event = #{name => XX, data => XX}
publish(Event) ->
    Name = maps:get(name, Event),
    Listeners = get({event_listeners, Name}),
    if
        Listeners =:= undefined orelse Listeners =:= [] -> ok;
        true ->
            NListeners = lists:foldl(fun(Listener, Acc) ->
                Callback = maps:get(callback, Listener),
                case Callback(Event) of
                    unsubscribe -> Acc;
                    _ -> [Listener|Acc]
                end
            end, [], Listeners),
            put({event_listeners, Name}, NListeners)
    end.

global_publish(Channel, Event) ->
    ?BROADCAST(Channel, global_event, Event).
