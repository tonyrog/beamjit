-module(test2).

-export([a/0, b/0, c/2]).

a() ->
    123.

b() ->
    124.

c(X,Y) when is_integer(X), is_integer(Y) ->
    Z = X+Y,
    Z + a() + b().



    
    

