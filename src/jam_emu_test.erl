%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Test jam_emu
%%% @end
%%% Created : 22 Aug 2022 by Tony Rogvall <tony@rogvall.se>

-module(jam_emu_test).

-export([test0/0, test1/0, test2/0, test3a/0, test3b/0]).
-export([test4/0, test4b/0, test4c/0]).

%% test arithmetic, add two arguments
test0() ->
    F = {function, exported, {test, foo, 2},
	 [{nodebug_info, 2, test, foo},
	  {arg,0},
	  {arg,1},
	  arith_plus,
	  ret
	  ]},
    jam_emu:test_call(F, [1,2]).

%% test message matching, 
%% receive the message term as passed in argument
test1() ->
    F = {function, exported, {test, recv, 1},
	 [{nodebug_info, 1, test, recv},
	  {label,1},
	  wait,
	  {try_me_else,{j,2}},
	  dup,
	  {eqArg,0},
	  popCommitJoin,
	  {pushAtom, true},
	  ret,
	  {label,2},
	  save,
	  pop,
	  {goto,{j,1}},
	  ret]},
    self() ! hello,
    self() ! crule,
    self() ! world,
    true = jam_emu:test_call(F, [world]),
    true = jam_emu:test_call(F, [hello]),
    true = jam_emu:test_call(F, [crule]),
    ok.


%% test message matching with timeout 
%% receive the message term as passed in argument
test2() ->
    F = {function, exported, {test, recv_tmo, 2},
	 [{nodebug_info, 2, test, recv_tmo},
	  {arg,0},setTimeout,
	  {label,1},
	  {wait1,{j,3}},
	  {try_me_else,{j,2}},
	  dup,
	  {eqArg,1},
	  popCommitJoin,
	  {pushAtom, true},
	  ret,
	  {label,2},
	  save,
	  pop,
	  {goto,{j,1}},
	  {label,3},
	  {pushAtom, timeout},
	  ret]},
    erlang:send_after(1000, self(), hello),
    true = jam_emu:test_call(F, [1100, hello]),
    erlang:send_after(1000, self(), crule),
    timeout = jam_emu:test_call(F, [900, crule]),
    erlang:send_after(1000, self(), world),
    true = jam_emu:test_call(F, [1100, world]),
    ok.

%% test catch 
%% receive the message term as passed in argument
test3a() ->
    F = {function, exported, {test, cat, 2},
	 [{nodebug_info, 2, test, cat},
	  {pushCatch,{j,1}},
	  {arg,0},
	  {arg,1},
	  arith_intdiv,
	  popCatch,
	  ret,
	  {label,1},
	  ret
	 ]},
    5 = jam_emu:test_call(F, [10, 2]),
    1 = jam_emu:test_call(F, [10, 9]),
    R = {'EXIT',_} = jam_emu:test_call(F, [10, 0]),
    R.


%% test catch / throw
%% receive the message term as passed in argument
test3b() ->
    F = {function, exported, {test, cat, 2},
	 [{nodebug_info, 2, test, cat},
	  {pushCatch,{j,1}},
	  {arg,0}, {arg,1}, mkList,
	  %% {call_remote, 1, {erlang, throw, 1}}
	  {call_bif, 1, {throw, 1}},
	  popCatch,
	  {pushAtom, false},
	  ret,
	  {label,1},
	  ret
	 ]},
    [1,2,3] = jam_emu:test_call(F, [[2,3], 1]).

%%
%% Test jamtobeam example
%%
%%    bar(A, Z) -> A+Z.
%%
%%    foo(X,Y,Z) ->
%%      A = X+Y*Z,
%%      B = bar(A,Z),
%%      A+B.
%%

test4() ->
    JAM = [{function, exported, {test, bar, 2},
	    [
	     {nodebug_info, 2, test, bar},
	     {arg,0},{arg,1},arith_plus,
	     ret
	    ]},
	   {function, exported, {test, foo, 3},
	    [
	     {nodebug_info, 3, test, foo},
	     {alloc,2},
	     {arg,0},
	     {arg,2},{arg,1},arith_times,
	     arith_plus,
	     {storeVar, 0},
	     {pushVar,0}, {arg,2}, {call_local, 2, {bar,2}},
	     {storeVar, 1},
	     {pushVar, 0},
	     {pushVar, 1},
	     arith_plus,
	     ret
	    ]}],
    CM = jam_emu:install(JAM, #{ module => test }),
    jam_emu:call(CM, foo, [1,2,3]).

test4b() ->
    %% make beam code of above
    beamjit:jbegin(),
    beamjit:module(test),
    beamjit:source("test.erl"),
    BarFuncL = beamjit:new_label(),
    BarEntryL = beamjit:new_label(),
    beamjit:bind(BarFuncL),
    beamjit:line([{location,"test.erl",1}]),
    beamjit:exported_function(test,bar,2,BarEntryL),
    beamjit:bind(BarEntryL),
    beamjit:allocate_heap_zero(2+2, 0, 2),
    beamjit:move({x,0}, {y,0}),
    beamjit:move({x,1}, {y,1}),
    beamjit:move({y,0}, {y,2}),
    beamjit:move({y,1}, {y,3}),    
    beamjit:add({y,2},{y,3},{y,2}),
    beamjit:move({y,2}, {x,0}),
    beamjit:deallocate(2+2),
    beamjit:return(),

    FooFuncL = beamjit:new_label(),
    FooEntryL = beamjit:new_label(),
    beamjit:bind(FooFuncL),
    beamjit:line([{location,"test.erl",3}]),
    beamjit:exported_function(test,foo,3,FooEntryL),
    beamjit:bind(FooEntryL),

    %% beamjit:allocate_heap(3+2+3, 0, 3),
    %% beamjit:init_yregs({list,[{y,3},{y,4},{y,5},{y,6},{y,7}]}),
    beamjit:allocate_heap_zero(3+2+3, 0, 3),
    %% save all arguments (because it is easy)
    beamjit:move({x,0}, {y,0}),
    beamjit:move({x,1}, {y,1}),
    beamjit:move({x,2}, {y,2}),
    %%
    beamjit:move({y,0}, {y,5}),
    beamjit:move({y,2}, {y,6}),
    beamjit:move({y,1}, {y,7}),
    beamjit:bif2({f,0}, '*',{y,6},{y,7},{y,6}),
    beamjit:bif2({f,0}, '+',{y,5},{y,6},{y,5}),
    beamjit:move({y,5}, {y,3}),
    beamjit:move({y,3}, {y,5}),
    beamjit:move({y,2}, {y,6}),
    beamjit:move({y,5}, {x,0}),
    beamjit:move({y,6}, {x,1}),
    beamjit:call(2, {f,BarEntryL}),
    beamjit:move({x,0}, {y,5}),
    beamjit:move({y,5}, {y,4}),
    beamjit:move({y,3}, {y,5}),
    beamjit:move({y,4}, {y,6}),
    beamjit:bif2({f,0}, '+',{y,5},{y,6},{y,5}),
    beamjit:move({y,5}, {x,0}),
    beamjit:deallocate(3+2+3),
    beamjit:return(),

    beamjit:int_code_end(),

    %% testing

    Binary = beamjit:build_module(),
    beamjit:jend(),

    code:load_binary(test, "test.beam", Binary), 
    test:foo(1,2,3).

    
%% Test automatic translation with jamtobeam

test4c() ->
    JAM = [{function, exported, {test, bar, 2},
	    [
	     {nodebug_info, 2, test, bar},
	     {arg,0},{arg,1},arith_plus,
	     ret
	    ]},
	   {function, exported, {test, foo, 3},
	    [
	     {nodebug_info, 3, test, foo},
	     {alloc,2},
	     {arg,0},
	     {arg,2},{arg,1},arith_times,
	     arith_plus,
	     {storeVar, 0},
	     {pushVar,0}, {arg,2}, {call_local, 2, {bar,2}},
	     {storeVar, 1},
	     {pushVar, 0},
	     {pushVar, 1},
	     arith_plus,
	     ret
	    ]}],
    jamtobeam:function_list(JAM).


    
