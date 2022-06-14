%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Test beamjit
%%% @end
%%% Created :  4 Jun 2022 by Tony Rogvall <tony@rogvall.se>

-module(beamjit_test).

-export([test/0]).

-define(TARGET_MOD, jit_test).

test() ->
    beamjit:jbegin(),
    beamjit:module(?TARGET_MOD),
    beamjit:source(?FILE),
    AddFuncL = beamjit:new_label(),
    AddEntryL = beamjit:new_label(),
    beamjit:bind(AddFuncL),
    beamjit:line([{location,"beam_jit_test.erl",?LINE}]),
    beamjit:exported_function(?TARGET_MOD,add,2,AddEntryL),
    beamjit:bind(AddEntryL),
    beamjit:add({x,0},{x,1},{x,0}),
    beamjit:return(),

    SubFuncL = beamjit:new_label(),
    SubEntryL = beamjit:new_label(),
    beamjit:bind(SubFuncL),
    beamjit:line([{location,"beam_jit_test.erl",?LINE}]),
    beamjit:exported_function(?TARGET_MOD,sub,2,SubEntryL),
    beamjit:bind(SubEntryL),
    beamjit:sub({x,0},{x,1},{x,0}),
    beamjit:return(),

    beamjit:int_code_end(),

    %% testing

    Binary = beamjit:build_module(),
    beamjit:jend(),

    %% debug the constructed chunks
    io:format("Mod = ~p\n", [Binary]),
    {ok,Chunks} = beamjit:load_binary(Binary),
    io:format("Chunks = ~p\n", [Chunks]),
    beamjit:print_chunks(Chunks),
    
    %% Load the built code and run the functions
    code:load_binary(?TARGET_MOD, "jit_test.beam", Binary), 

    io:format("~s:add(~w,~w) = ~w\n", [?TARGET_MOD, 13, 17,
				       ?TARGET_MOD:add(13, 17)]),
    io:format("~s:sub(~w,~w) = ~w\n", [?TARGET_MOD, 13, 17,
				       ?TARGET_MOD:sub(13, 17)]),
    ok.

    

