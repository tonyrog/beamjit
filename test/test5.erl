-module(test5).
-export([a/1]).

a(X) when is_list(X) ->
    erlang:send(self(), foo),
    self() ! bar,
    A1 = erlang:hd(X),
    B = erlang:read_timer(erlang:make_ref()),
    Ref = erlang:start_timer(self(), 1000, baz),
    {Ref, A1,B}.

    
    
