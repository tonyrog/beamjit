%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Test jamjit
%%% @end
%%% Created : 17 Aug 2022 by Tony Rogvall <tony@rogvall.se>

-module(jamjit_test).

-export([test/0]).

-define(TARGET_MOD, jit_test).

test() ->
    jamjit:jbegin(),
    jamjit:module(?TARGET_MOD),
    jamjit:source(?FILE),

    jamjit:exported_function(?TARGET_MOD,add,2),
    jamjit:pushInt(12),
    jamjit:pushInt(13),
    jamjit:arith_plus(),
    jamjit:ret(),

    jamjit:exported_function(?TARGET_MOD,sub,2),
    jamjit:pushInt(12),
    jamjit:pushInt(13),
    jamjit:arith_minus(),
    jamjit:ret(),

    jamjit:jend().

