-module(test1).

-export([a/0, b/0, c/2]).

a() ->
    math:pi().

b() ->
    3.14.

c(X,Y) when is_float(X), is_float(Y) ->
    Z = X+Y,
    Z + 1.0.


    
    

