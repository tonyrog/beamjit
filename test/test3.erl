-module(test3).

-export([a/0, c/2, d/2]).


a() ->
    12.

b() ->
    23.

c(X,Y) when is_integer(X), is_float(Y) ->
    Z = X+Y,
    Z + b().

d(A,B) when is_float(A), is_float(B) ->
    (-A*(A+B))/(A-B).

