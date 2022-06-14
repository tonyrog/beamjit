-module(test4).

-export([a/0, b/0, c/2]).

a() ->
    "hello".

b() ->
    <<"world">>.

c("abc"++S1,S2) ->
    S = S1 ++ a() ++ binary_to_list(b()) ++ S2,
    d(),
    S.

-include("./done.hrl").
