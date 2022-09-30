%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    JAM emulator
%%% @end
%%% Created : 17 Aug 2022 by Tony Rogvall <tony@rogvall.se>

-module(jam_emu).

-export([load/1]).
-export([install/2]).
-export([call/3]).
-export([test_call/2]).

%% test message passing
-export([set_timeout/1, clr_timeout/0, read_timeout/0]).
-export([message_init/0,
	 message_save/0,
	 message_next/1,
	 message_remove/0]).

-define(verbose(F,A), io:format((F),(A))).
%% -define(verbose(F,A), ok).
-define(MSG,       '$msg').
-define(MSG_N,     '$msg_n').
-define(MSG_POS,   '$msg_pos').
-define(MSG_TIMER, '$msg_timer').      %% current timer reference
-define(MSG_TIMEOUT, '$msg_timeout').  %% current timeout value

%% jam max timeout was (1 bsl 27) - 1 (beam has (1 bsl 32)-1)
-define(is_timeout_value(X), (((X) band (bnot 16#7ffffff)) =:= 0)).

load(File) ->
    {ok, JAM} = jamjit:load_file(File),
    [{function,_Type,{Mod,_,_},_Code}|_] = JAM,
    install(JAM, #{ module => Mod }).

test_call(F={function,_Type,{_Mod,Func,Arity},_Code}, Args) ->
    CM = install([F], #{ module => test }),
    message_init(),
    Arity = length(Args), %% fixme match
    FA = {Func,Arity},
    %%Type = maps:get({type,FA}, CM),
    Code = maps:get({function,FA}, CM),
    call_(FA,Args,Code,CM).
    
    
install([{function,Type,{Mod,Func,Arity},Code}|Fs], CM) ->
    #{ module := Mod } = CM,
    Code1 = install_code(Code, [], entry, #{}),
    install(Fs, CM#{ {function,{Func,Arity}} => Code1,
		    {type,{Func,Arity}} => Type
		  });
install([], CM) ->
    CM.

install_code([{label,L}|Code], Acc, L0, CM) ->
    Segment = lists:reverse([{goto,{j,L}}|Acc]),
    install_code(Code, [], L, CM#{ L0 => Segment });
install_code([Instr|Code], Acc, L0, CM) ->
    install_code(Code, [Instr|Acc], L0, CM);
install_code([], Acc, L0, CM) ->
    CM#{ L0 => lists:reverse(Acc) }.

call(CM, Func, Args) ->
    message_init(),  %% setup message queue globals
    %% locate Func/arity among JAM functions
    Arity = length(Args),
    FA = {Func,Arity},
    Type = maps:get({type,FA}, CM),
    if Type =:= exported ->
	    Code = maps:get({function,FA}, CM),
	    call_(FA,Args,Code,CM);
       true ->
	    error({function,FA,not_exported})
    end.

-define(FUNCTION_CLAUSE, -1).
-define(BADMATCH,         0).

call_(FA,Args,Code,CM) ->
    ?verbose("call ~p args=~p\n", [FA,Args]),
    Segment = maps:get(entry, Code),
    S = [],                   %% empty stack
    F = {},                   %% empty frame
    A = list_to_tuple(Args),  %% array of arguments
    Fail = ?FUNCTION_CLAUSE,  %% no guard
    emu(Segment,S,F,A,FA,Fail,Code,CM).

emu([{goto,{j,J}}],S,F,A,FA,Fail,Code,CM) ->
    goto(J,S,F,A,FA,Fail,Code,CM);
emu([ret|_],[R|_S],_F,_A,_FA,_Fail,_Code,_CM) ->
    R;
emu([I|Is],S,F,A,FA,L,Code,CM) ->
    ?verbose("exec: ~p, stack=~p\n", [I, S]),
    case I of
	{debug_info,_Arity,_Mod,_Func} -> 
	    emu(Is,S,F,A,FA,L,Code,CM);
	{nodebug_info,_Arity,_Mod,_Func} ->
	    emu(Is,S,F,A,FA,L,Code,CM);
	{try_me_else,{j,L1}} -> 
	    emu(Is,S,F,A,FA,L1,Code,CM);
	try_me_else_fail ->
	    emu(Is,S,F,A,FA,?FUNCTION_CLAUSE,Code,CM);
	commit ->
	    emu(Is,S,F,A,FA,?BADMATCH,Code,CM);
	popCommit ->
	    [_|S1] = S,
	    emu(Is,S1,F,A,FA,?BADMATCH,Code,CM);
	{alloc,N} ->
	    emu(Is,S,erlang:make_tuple(N,[]),A,FA,L,Code,CM);
	{arg,J} ->
	    emu(Is,[element(J+1,A)|S],F,A,FA,L,Code,CM);
	{pushVar,J} ->
	    emu(Is,[element(J+1,F)|S],F,A,FA,L,Code,CM);
	self ->
	    emu(Is,[self()|S],F,A,FA,L,Code,CM);
	send ->
	    [Message,Pid|S1] = S,
	    if is_pid(Pid); is_atom(Pid) ->
		    Result = Pid ! Message,
		    emu(Is,[Result|S1],F,A,FA,L,Code,CM);
	       true ->
		    fail(S,F,A,FA,L,badarg,Code,CM)
	    end;
	pushNil ->
	    emu(Is,[[]|S],F,A,FA,L,Code,CM);
	{pushAtom,Atm} ->
	    emu(Is,[Atm|S],F,A,FA,L,Code,CM);
	{pushStr,Str} ->
	    emu(Is,[Str|S],F,A,FA,L,Code,CM);
	{pushInt,Int} ->
	    emu(Is,[Int|S],F,A,FA,L,Code,CM);
	{pushFloat,Flt} ->
	    emu(Is,[Flt|S],F,A,FA,L,Code,CM);
	{mkTuple,N} ->
	    {T0,S1} = pop_args(N, S),
	    T1 = list_to_tuple(T0),
	    emu(Is,[T1|S1],F,A,FA,L,Code,CM);
	mkList ->
	    [H,T|S1] = S,
	    emu(Is,[[H|T]|S1],F,A,FA,L,Code,CM);
	pop ->
	    [_|S1] = S,
	    emu(Is,S1,F,A,FA,L,Code,CM);
	dup -> 
	    [H|_] = S,
	    emu(Is,[H|S],F,A,FA,L,Code,CM);
	{stack_need,_Need} -> %% no need in emulator
	    emu(Is,S,F,A,FA,L,Code,CM);	
	{head_need,_Need} -> %% no need in emulator
	    emu(Is,S,F,A,FA,L,Code,CM);	
	{storeVar,J} ->
	    [T1|S1] = S,
	    emu(Is,S1,setelement(J+1,F,T1),A,FA,L,Code,CM);
	getNil ->
	    testop(fun(X) -> X =:= [] end,Is,S,F,A,FA,L,Code,CM);
	{getInt,Int} ->
	    testop(fun(X) -> X =:= Int end,Is,S,F,A,FA,L,Code,CM);
	{getFloat,Flt} ->
	    testop(fun(X) -> X =:= Flt end,Is,S,F,A,FA,L,Code,CM);
	{getAtom,Atm} ->
	    testop(fun(X) -> X =:= Atm end,Is,S,F,A,FA,L,Code,CM);
	{getStr,Str} ->
	    testop(fun(X) -> X =:= Str end,Is,S,F,A,FA,L,Code,CM);
	list_length ->
	    lstuop(fun(X) -> length(X) end,Is,S,F,A,FA,L,Code,CM);
	head ->
	    lstuop(fun(X) -> hd(X) end,Is,S,F,A,FA,L,Code,CM);
	tail ->
	    lstuop(fun(X) -> tl(X) end,Is,S,F,A,FA,L,Code,CM);
	unpkList ->
	    case S of
		[[H|T]|S1] ->
		    emu(Is,[H,T|S1],F,A,FA,L,Code,CM);
		_ ->
		    fail(S,F,A,FA,L,Code,CM)
	    end;
	{unpkTuple,N} ->
	    case S of
		[T|S1] when tuple_size(T) =:= N ->
		    emu(Is,tuple_to_list(T)++S1,F,A,FA,L,Code,CM);
		_ ->
		    fail(S,F,A,FA,L,Code,CM)
	    end;
	{enter_local,Arity,FA1={_Func,Arity}} ->
	    {Args1,_SS} = pop_args(Arity,S), 
	    Code1 = maps:get({function,FA1}, CM),
	    Segment1 = maps:get(entry, Code1),
	    S1 = [],                   %% empty stack
	    F1 = {},                   %% empty frame
	    A1 = list_to_tuple(Args1), %% array of arguments
	    Fail1 = ?FUNCTION_CLAUSE,  %% no guard
	    emu(Segment1,S1,F1,A1,FA1,Fail1,Code1,CM);

	{call_local,Arity,FA1={_Func,Arity}} ->
	    {Args1,SS} = pop_args(Arity,S), 
	    Code1 = maps:get({function,FA1}, CM),
	    Segment1 = maps:get(entry, Code1),
	    S1 = [],                   %% empty stack
	    F1 = {},                   %% empty frame
	    A1 = list_to_tuple(Args1), %% array of arguments
	    Fail1 = ?FUNCTION_CLAUSE,  %% no guard
	    T = emu(Segment1,S1,F1,A1,FA1,Fail1,Code1,CM),
	    emu(Is,[T|SS],F,A,FA,L,Code,CM);

	apply_call ->
	    [Args,Func,Mod|S1] = S,
	    try length(Args) of
		_Arity when is_atom(Func), is_atom(Mod) ->
		    T = apply(Mod, Func, Args),
		    emu(Is,[T|S1],F,A,FA,L,Code,CM);
		_ ->
		    fail(S,F,A,FA,L,badarg,Code,CM)
	    catch
		error:Reason ->
		    fail(S,F,A,FA,L,Reason,Code,CM)
	    end;

	apply_enter ->
	    [Args,Func,Mod|_S1] = S,
	    try length(Args) of
		_Arity when is_atom(Func), is_atom(Mod) ->
		    apply(Mod, Func, Args);
		_ ->
		    fail(S,F,A,FA,L,badarg,Code,CM)
	    catch
		error:Reason ->
		    fail(S,F,A,FA,L,Reason,Code,CM)
	    end;

	{call_remote,Arity,{Mod,Func,Arity}} ->
	    {Args1,S1} = pop_args(Arity,S), 
	    T = apply(Mod, Func, Args1),
	    emu(Is,[T|S1],F,A,FA,L,Code,CM);

	{enter_remote,Arity,{Mod,Func,Arity}} ->
	    {Args1,_S1} = pop_args(Arity,S),
	    apply(Mod, Func, Args1);

	{call_bif,Arity,{Func,Arity}} ->
	    {Args1,S1} = pop_args(Arity,S), 
	    T = apply(erlang, Func, Args1),
	    emu(Is,[T|S1],F,A,FA,L,Code,CM);

	{enter_bif,Arity,{Func,Arity}} ->
	    {Args1,_S1} = pop_args(Arity, S),
	    apply(erlang, Func, Args1);

	arith_plus -> mixbop(fun(X,Y) -> X+Y end,Is,S,F,A,FA,L,Code,CM);
	arith_minus -> mixbop(fun(X,Y) -> X-Y end,Is,S,F,A,FA,L,Code,CM);
	arith_times -> mixbop(fun(X,Y) -> X*Y end,Is,S,F,A,FA,L,Code,CM);
	arith_div   -> mixbop(fun(X,Y) -> X/Y end,Is,S,F,A,FA,L,Code,CM);
	arith_neg   -> mixuop(fun(X) -> -X end,Is,S,F,A,FA,L,Code,CM);
	arith_intdiv -> intbop(fun(X,Y) -> X div Y end,Is,S,F,A,FA,L,Code,CM);
	arith_band -> intbop(fun(X,Y) -> X band Y end,Is,S,F,A,FA,L,Code,CM);
	arith_bor -> intbop(fun(X,Y) -> X bor Y end,Is,S,F,A,FA,L,Code,CM);
	arith_bxor -> intbop(fun(X,Y) -> X bxor Y end,Is,S,F,A,FA,L,Code,CM);
	arith_bnot -> intuop(fun(X) -> bnot X end,Is,S,F,A,FA,L,Code,CM);
	arith_bsl -> intbop(fun(X,Y) -> X bsl Y end,Is,S,F,A,FA,L,Code,CM);
	arith_bsr -> intbop(fun(X,Y) -> X bsr Y end,Is,S,F,A,FA,L,Code,CM);
	arith_rem -> intbop(fun(X,Y) -> X rem Y end,Is,S,F,A,FA,L,Code,CM);

	{eqVar,J} ->
	    Var = element(J+1,F),
	    case S of
		[Var|S1] ->
		    emu(Is,S1,F,A,FA,L,Code,CM);
		_ ->
		    fail(S,F,A,FA,L,Code,CM)
	    end;
	{eqArg,J} ->
	    Arg = element(J+1,A),
	    case S of
		[Arg|S1] ->
		    emu(Is,S1,F,A,FA,L,Code,CM);
		_ ->
		    fail(S,F,A,FA,L,Code,CM)
	    end;
	comp_gt -> cmpop(fun(X,Y) -> X > Y end,Is,S,F,A,FA,L,Code,CM);
	comp_lt -> cmpop(fun(X,Y) -> X < Y end,Is,S,F,A,FA,L,Code,CM);
	comp_geq -> cmpop(fun(X,Y) -> X >= Y end,Is,S,F,A,FA,L,Code,CM);
	comp_leq -> cmpop(fun(X,Y) -> X =< Y end,Is,S,F,A,FA,L,Code,CM);
	comp_eqeq -> cmpop(fun(X,Y) -> X == Y end,Is,S,F,A,FA,L,Code,CM);
	comp_neq -> cmpop(fun(X,Y) -> X /= Y end,Is,S,F,A,FA,L,Code,CM);
	exact_eqeq -> cmpop(fun(X,Y) -> X =:= Y end,Is,S,F,A,FA,L,Code,CM);
	exact_neq -> cmpop(fun(X,Y) -> X =/= Y end,Is,S,F,A,FA,L,Code,CM);
	test_float -> testop(fun(X) -> is_float(X) end,Is,S,F,A,FA,L,Code,CM);
	test_atom -> testop(fun(X) -> is_atom(X) end,Is,S,F,A,FA,L,Code,CM);
	test_tuple -> testop(fun(X) -> is_tuple(X) end,Is,S,F,A,FA,L,Code,CM);
	test_pid -> testop(fun(X) -> is_pid(X) end,Is,S,F,A,FA,L,Code,CM);
	test_port -> testop(fun(X) -> is_port(X) end,Is,S,F,A,FA,L,Code,CM);
	test_reference -> testop(fun(X) -> is_reference(X) end,Is,S,F,A,FA,L,Code,CM);
	test_binary -> testop(fun(X) -> is_binary(X) end,Is,S,F,A,FA,L,Code,CM);
	test_list -> testop(fun(X) -> is_list(X) end,Is,S,F,A,FA,L,Code,CM);
	test_integer -> testop(fun(X) -> is_integer(X) end,Is,S,F,A,FA,L,Code,CM);
	test_number -> testop(fun(X) -> is_number(X) end,Is,S,F,A,FA,L,Code,CM);
	test_constant -> testop(fun(X) -> 
					is_integer(X) orelse 
					    is_float(X) orelse
					    is_binary(X) orelse
					    is_atom(X) orelse
					    is_reference(X) orelse
					    is_port(X) orelse
					    is_pid(X)
				end,
				Is,S,F,A,FA,L,Code,CM);	
	{type,Mask} ->
	    case S of
		[Val|S1] ->
		    F = 
			fun(integer, X) -> is_integer(X);
			   (float, X) -> is_float(X);
			   (atom, X) -> is_atom(X);
			   (reference, X) -> is_reference(X);
			   (port, X) -> is_port(X);
			   (pid, X) -> is_pid(X);
			   (tuple, X) -> is_tuple(X);
			   (nil, X) -> X =:= [];
			   (list,X) -> is_list(X) andalso X =/= [];
			   (binary,X) -> is_binary(X)
			end,
		    case lists:any(fun(Type) -> F(Type, Val) end, Mask) of
			true ->
			    emu(Is,S1,F,A,FA,L,Code,CM);
			false ->
			    fail(S,F,A,FA,L,Code,CM)
		    end;
		_ ->
		    fail(S,F,A,FA,L,Code,CM)
	    end;
	failFunction ->
	    fail(S,F,A,FA,L,{function_clause,FA},Code,CM);
	failCase ->
	    fail(S,F,A,FA,L,{case_clause,hd(S)},Code,CM);
	failIf ->
	    fail(S,F,A,FA,L,if_clause,Code,CM);
	{goto,{j,J}} ->
	    goto(J,S,F,A,FA,L,Code,CM);
	{gotoix,Low,Table} ->
	    [Val|S1] = S,
	    Index = (Val - Low)+1,
	    if Index < 1; Index > tuple_size(Table) ->
		    fail(S,F,A,FA,L,Code,CM);
	       true ->
		    {j,J} = element(Index, Table),
		    goto(J,S1,F,A,FA,L,Code,CM)
	    end;
	{hash, K} ->
	    [Term|S1] = S,
	    %% fixme: must use the old "buggy" hashed code
	    H = erlang:phash(Term, K),
	    emu(Is,[H|S1],F,A,FA,L,Code,CM);
	{pushCatch,{j,J}} ->
	    try emu(Is,S,F,A,FA,L,Code,CM) of
		{S1,Is1} ->
		    emu(Is1,S1,F,A,FA,L,Code,CM)
	    catch
		throw:Val ->
		    goto(J,[Val|S],F,A,FA,L,Code,CM);
		error:Reason ->
		    goto(J,[{'EXIT',Reason}|S],F,A,FA,L,Code,CM)
	    end;
	popCatch ->
	    {S,Is};
	save -> %% move to next message in mbox
	    message_save(),
	    emu(Is,S,F,A,FA,L,Code,CM);
	wait ->
	    case message_next(infinity) of
		{message,Message} ->
		    emu(Is,[Message|S],F,A,FA,L,Code,CM)
	    end;
	{wait1,{j,J}} ->
	    Tmo = read_timeout(),
	    case message_next(Tmo) of
		{message,Message} ->
		    emu(Is,[Message|S],F,A,FA,L,Code,CM);
		timeout ->
		    message_init(),
		    goto(J,S,F,A,FA,?FUNCTION_CLAUSE,Code,CM)
	    end;
	setTimeout ->
	    case S of
		[Tmo|S1] when ?is_timeout_value(Tmo); Tmo =:= infinity ->
		    set_timeout(Tmo),
		    emu(Is,S1,F,A,FA,L,Code,CM);
		_ ->
		    fail(S,F,A,FA,L,badarg,Code,CM)
	    end;
	popCommitJoin -> %% remove current message from mbox
            message_remove(),
	    message_init(),
	    [_|S1] = S,
	    emu(Is,S1,F,A,FA,?BADMATCH,Code,CM)
    end.

%% [Xn-1,...X0 | S1] -> reverse(Args) = [X0,...Xn1]
pop_args(N, S) ->
    {Args,S1} = lists:split(N,S),
    {lists:reverse(Args), S1}.



intbop(Fun,Is,[X,Y|S],F,A,FA,L,Code,CM) ->
    if is_integer(X), is_integer(Y) ->
	    try Fun(Y,X) of
		Z -> emu(Is,[Z|S],F,A,FA,L,Code,CM)
	    catch
		error:_ -> %% fixme: correct error code!
		    fail(S,F,A,FA,L,badarith,Code,CM)
	    end;
       true ->
	    fail(S,F,A,FA,L,Code,CM)
    end.

lstuop(Fun,Is,[X|S],F,A,FA,L,Code,CM) ->
    if is_list(X) ->
	    try Fun(X) of
		Z -> emu(Is,[Z|S],F,A,FA,L,Code,CM)
	    catch
		error:Reason ->
		    fail(S,F,A,FA,L,Reason,Code,CM)
	    end;
       true ->
	    fail(S,F,A,FA,L,badarg,Code,CM)
    end.

intuop(Fun,Is,[X|S],F,A,FA,L,Code,CM) ->
    if is_integer(X) ->
	    try Fun(X) of
		Z -> emu(Is,[Z|S],F,A,FA,L,Code,CM)
	    catch
		error:_Reason ->
		    fail(S,F,A,FA,L,badarith,Code,CM)
	    end;
       true ->
	    fail(S,F,A,FA,L,badarg,Code,CM)
    end.

mixuop(Fun,Is,[X|S],F,A,FA,L,Code,CM) ->
    if is_number(X) ->
	    try Fun(X) of
		Z -> emu(Is,[Z|S],F,A,FA,L,Code,CM)
	    catch
		error:_Reason ->
		    fail(S,F,A,FA,L,badarith,Code,CM)
	    end;
       true ->
	    fail(S,F,A,FA,L,badarg,Code,CM)
    end.
    
mixbop(Fun,Is,[X,Y|S],F,A,FA,L,Code,CM) ->
    if is_number(X), is_number(Y) ->
	    try Fun(Y,X) of
		Z -> emu(Is,[Z|S],F,A,FA,L,Code,CM)
	    catch
		error:_Reason -> %% fixme: correct error code!
		    fail(S,F,A,FA,L,badarith,Code,CM)
	    end;
       true ->
	    fail(S,F,A,FA,L,badarg,Code,CM)
    end.

cmpop(Fun,Is,S=[X,Y|S1],F,A,FA,L,Code,CM) ->
    case Fun(Y,X) of
	true ->
	    emu(Is,S1,F,A,FA,L,Code,CM);
	false ->
	    fail(S,F,A,FA,L,Code,CM)
    end.

testop(Fun,Is,S=[X|S1],F,A,FA,L,Code,CM) ->
    case Fun(X) of
	true ->
	    emu(Is,S1,F,A,FA,L,Code,CM);
	false ->
	    fail(S,F,A,FA,L,Code,CM)
    end.

goto(J,S,F,A,FA,Fail,Code,CM) ->
    Segment = maps:get(J, Code,CM),
    emu(Segment,S,F,A,FA,Fail,Code,CM).

fail(S,F,A,FA,Fail,Code,CM) when Fail > 0 ->
    goto(Fail,S,F,A,FA,?FUNCTION_CLAUSE,Code,CM);
fail(_S,_F,_A,_FA,_Fail,_Code,_CM) ->
    error(function_clause).

fail(S,F,A,FA,Fail,_Reason,Code,CM) when Fail > 0 ->
    goto(Fail,S,F,A,FA,?FUNCTION_CLAUSE,Code,CM);
fail(_S,_F,_A,_FA,_Fail,Reason,_Code,_CM) ->
    error(Reason).

%% Message passing emulation (that should work with native process)

set_timeout(infinity) ->
    clr_timeout(),
    put(?MSG_TIMEOUT, infinity);
set_timeout(0) ->
    clr_timeout(),
    put(?MSG_TIMEOUT, 0);
set_timeout(Tmo) when is_integer(Tmo), Tmo > 0 ->
    clr_timeout(),
    T = erlang:start_timer(Tmo, undefined, undefined),
    put(?MSG_TIMEOUT, Tmo),
    put(?MSG_TIMER, T).

clr_timeout() ->
    erase(?MSG_TIMEOUT),
    case get(?MSG_TIMER) of
	undefined -> ok;
	Ref when is_reference(Ref) ->
	    erlang:cancel_timer(Ref),
	    erase(?MSG_TIMER)
    end.

read_timeout() ->
    case get(?MSG_TIMEOUT) of
	0 -> 0;
	infinity -> infinity;
	_ ->
	    case erlang:read_timer(get(?MSG_TIMER)) of
		false -> 0;
		TimeLeft -> TimeLeft
	    end
    end.

%% initialaize message processing
message_init() ->
    erase(?MSG),
    erase(?MSG_N),
    erase(?MSG_POS),
    erase(?MSG_TIMER),
    erase(?MSG_TIMEOUT).

message_save() ->
    Pos = message_pos(),
    put(?MSG_POS, Pos+1).

message_next(Timeout) ->
    case message_iter(timeout, 0, nomatch) of
	Message = {message,_} -> Message;
	timeout when Timeout =:= 0 -> timeout;
	timeout -> message_iter(timeout, Timeout, nomatch)
    end.

message_remove() ->
    case message_iter(timeout, 0, true) of
	{message,_Message} -> true;
	timeout -> false
    end.

message_iter(Default, Timeout, Match) ->
    put(?MSG_N, message_pos() + 1),  %% number of messages to scan
    put(?MSG, Default),
    prim_eval:'receive'(
      fun(Message) ->
	      case get(?MSG_N)-1 of
		  -1 ->
		      nomatch;
		  0 ->
		      put(?MSG_N, 0),
		      put(?MSG,{message,Message}),
		      Match;
		  N ->
		      put(?MSG_N, N),
		      nomatch
	      end
      end, Timeout),
    get(?MSG).

message_pos() ->
    case get(?MSG_POS) of
	undefined -> 0;
	Pos -> Pos
    end.
