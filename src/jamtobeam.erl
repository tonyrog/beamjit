%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    convert expressions into beam assembler
%%% @end
%%% Created : 21 Jun 2022 by Tony Rogvall <tony@rogvall.se>

-module(jamtobeam).
-compile(export_all).
-export([file/1, function_list/1]).

-record(ra,
	{
	 argn,         %% y(0)...y(argn-1) are function arguments
	 varn = 0,     %% y(argn)...y(argn+varn-1) are locals (set by alloc)
	 top = 0,      %% top of stack positoin y(argn) ytop(top-1)...
	 maxstk = 0,   %% maximum stack size used
	 inguard = false, %% try_me_else/try_me_else_fail
	 reason = badmatch,
	 fail = {f,0}
	}).

file(Filename) ->
    {ok,Jam} = jamjit:load_file(Filename),
    function_list(Jam).

function_list(Jam) ->
    function_list(Jam, []).

function_list([{function,_Type,{Mod,Func,Arity},Code}|Seq], Beam) ->
    {Ra,Is} = function(Code, Arity),
    Is1 = fixup(Is, Ra),
    B = [{func_info,{Mod,Func,Arity}}|Is1],
    function_list(Seq, [B|Beam]);
function_list([], Beam) ->
    lists:reverse(Beam).

function(Seq,Arity) when is_integer(Arity), Arity >= 0 ->
    Ra = #ra { argn = Arity },
    %% Move arguments into y register 0..Arity-1
    %% we could scratch x registers here and set Live=0 in succeding bif calls
    %% (since they are move into y registers)
    %% in the prelude, but we may want to optimise the code soon, so save them
    {Ra1, Is} = seq(Seq, Ra), %% run body
    {Ra1, [alloc | [{move,{x,I},{y,I}} || I <- lists:seq(0,Arity-1)]] ++ Is}.

%% add stack allocation/deallocation info when we know max stack usage
fixup([alloc | Is], Ra) ->
    Alloc = Ra#ra.argn + Ra#ra.varn + Ra#ra.maxstk,
    [{allocate_heap_zero,Alloc,0,Ra#ra.argn} | fixup(Is, Ra)];
fixup([dealloc | Is], Ra) ->
    Alloc = Ra#ra.argn + Ra#ra.varn + Ra#ra.maxstk,
    [{deallocate, Alloc} | fixup(Is, Ra)];
fixup([I | Is], Ra) ->
    [I | fixup(Is, Ra)];
fixup([], _Ra) ->
    [].

seq([{nodebug_info,_Arity,_Mod,_Func}|Exprs], Ra) ->
    seq(Exprs, Ra);
seq([{debug_info,_Arity,_Mod,_Func}|Exprs], Ra) ->
    seq(Exprs, Ra);
seq([{alloc,N}|Exprs], Ra) ->
    seq(Exprs, Ra#ra{varn=N });
seq([{try_me_else,J}|Exprs], Ra) ->
    seq(Exprs, Ra#ra{fail=J,inguard=true});
seq([try_me_else_fail|Exprs], Ra) ->
    seq(Exprs, Ra#ra{fail={f,0},reason=function_clause,inguard=true});
seq([commit|Exprs], Ra) ->
    seq(Exprs, Ra#ra{fail={f,0},
			    reason=badmatch,inguard=false});
seq([popCommit|Exprs], Ra) ->
    Ra1 = pop(Ra),
    seq(Exprs,Ra1#ra{fail={f,0},reason=badmatch,inguard=false});
seq([popCommitJoin|Exprs], Ra) ->
    Ra1 = pop(Ra),
    seq(Exprs,Ra1#ra{fail={f,0},reason=badmatch,inguard=false});

seq([pop|Exprs], Ra) ->
    seq(Exprs,pop(Ra));
seq([dup|Exprs], Ra=#ra{top=Top,maxstk=Stk}) ->
    {Ra1,Is1} = seq(Exprs,Ra#ra{top=Top+1,maxstk=max(Stk,Top+1)}),
    {Ra1, [{move,top(Ra),stk(-1,Ra)} | Is1]};
seq([{stack_need,_Need}|Exprs], Ra) ->
    %% FIXME - check 1024 limit!
    seq(Exprs, Ra);

seq([Expr|Exprs], Ra) ->
    {Ra1,Is1} = expr(Expr,Ra),
    {Ra2,Is2} = seq(Exprs,Ra1),
    {Ra2,Is1++Is2};
seq([], Ra) ->
    {Ra, []};
seq(Expr, Ra) ->
    expr(Expr, Ra).

%% yreg used for arg/var and stack
arg(I,_Ra) -> {y,I}.
var(I,Ra) -> {y,Ra#ra.argn+I}.
stk(I,Ra) -> {y,Ra#ra.argn+Ra#ra.varn+Ra#ra.top-I-1}.
top(Ra) -> stk(0, Ra).
snd(Ra) -> stk(1, Ra).
    

expr({pushInt,I}, Ra) -> push({integer,I}, Ra);
expr({floatFloat,F}, Ra) -> push({float,F}, Ra);
expr({pushAtom,A}, Ra) -> push({atom,A}, Ra);
expr({pushStr,S}, Ra) -> push({literal,S}, Ra);
expr(pushNil, Ra) -> push([], Ra);
expr({arg,I}, Ra) -> push(arg(I,Ra), Ra);
expr({pushVar,J}, Ra) -> push(var(J,Ra), Ra);
expr({storeVar,J}, Ra) -> store(var(J,Ra), Ra);
expr({eqArg,I}, Ra) -> eq(arg(I,Ra), Ra);
expr({eqVar,J}, Ra) -> eq(var(J,Ra), Ra);
expr(Test=test_integer,Ra) -> test(Test,Ra);
expr(Test=test_float,Ra) -> test(Test,Ra);
expr(Test=test_number,Ra) -> test(Test,Ra);
expr(Test=test_atom,Ra) -> test(Test,Ra);
expr(Test=test_pid,Ra) -> test(Test,Ra);
expr(Test=test_reference,Ra) -> test(Test,Ra);
expr(Test=test_port,Ra) -> test(Test,Ra);

expr(Cmp=comp_gt,Ra) -> compare(Cmp, Ra);
expr(Cmp=comp_lt,Ra) -> compare(Cmp, Ra);
expr(Cmp=comp_geq,Ra) -> compare(Cmp, Ra);
expr(Cmp=comp_leq,Ra) -> compare(Cmp, Ra);
expr(Cmp=comp_eqeq,Ra) -> compare(Cmp, Ra);
expr(Cmp=comp_neq,Ra) -> compare(Cmp, Ra);

expr(Exact=exact_eqeq,Ra) -> exact(Exact, Ra);
expr(Exact=exact_neq,Ra) -> exact(Exact, Ra);

expr(Test=test_constant,Ra) -> test(Test,Ra);
expr(Test=test_list,Ra) -> test(Test,Ra);
expr(Test=test_tuple,Ra) -> test(Test,Ra);

expr(getNil,Ra=#ra{fail=Fail}) ->
    {pop(Ra), [{is_eq_exact,Fail,top(Ra),[]}]};
expr({getInt,I},Ra=#ra{fail=Fail}) ->
    {pop(Ra), [{is_eq_exact,Fail,top(Ra),{integer,I}}]};
expr({getFloat,F},Ra=#ra{fail=Fail}) ->
    {pop(Ra), [{is_eq_exact,Fail,top(Ra),{float,F}}]};
expr({getAtom,A},Ra=#ra{fail=Fail}) ->
    {pop(Ra), [{is_eq_exact,Fail,top(Ra),{atom,A}}]};
expr({getStr,Str},Ra=#ra{fail=Fail}) ->
    {pop(Ra), [{is_eq_exact,Fail,top(Ra),{literal,Str}}]};

expr(Op=arith_plus,Ra) -> arith2(Op,Ra);
expr(Op=arith_minus,Ra) -> arith2(Op,Ra);
expr(Op=arith_times,Ra) -> arith2(Op,Ra);
expr(Op=arith_div,Ra) -> arith2(Op,Ra);
expr(Op=arith_intdiv,Ra) -> arith2(Op,Ra);
expr(Op=arith_band,Ra) -> arith2(Op,Ra);
expr(Op=arith_bor,Ra) -> arith2(Op,Ra);
expr(Op=arith_bxor,Ra) -> arith2(Op,Ra);
expr(Op=arith_bsl,Ra) -> arith2(Op,Ra);
expr(Op=arith_bsr,Ra) -> arith2(Op,Ra);
expr(Op=arith_rem,Ra) -> arith2(Op,Ra);

expr(Op=arith_bnot,Ra) -> arith1(Op,Ra);
expr(Op=arith_neg,Ra) -> arith1(Op,Ra);

expr({mkTuple,N},Ra) ->
    {pop(N-1, Ra),
     [{put_tuple2,stk(N-1,Ra),
       {list,[stk(I,Ra) || I <- lists:seq(N-1,0,-1)]}}]};

expr({unpkTuple,N},Ra=#ra{top=Top,maxstk=Stk}) ->
    {Ra#ra{top=Top+N-1, maxstk=max(Top+N-1,Stk)},
     [{get_tuple_element,
       top(Ra),stk(I,Ra)} || I <- lists:seq(N-1,0,-1)]};

expr(mkList,Ra) ->
    {pop(Ra), 
     [{put_list,snd(Ra),top(Ra),snd(Ra)}]};
expr(unpkList,Ra=#ra{top=Top,maxstk=Stk,fail=Fail}) ->
    {Ra#ra{top=Top+1,maxstk=max(Top+1,Stk)},
     [{is_nonempty_list,Fail,top(Ra)},
      {get_list,top(Ra),snd(Ra),top(Ra)}]};

expr(ret, Ra) ->
    {Ra1,Is1} = store({x,0}, Ra),
    {Ra1, Is1++[dealloc,return]};
expr(list_length, Ra=#ra{fail=Fail}) ->
    {Ra, [bif1(length,Fail,Ra)]};
expr(self, Ra=#ra{top=Top,maxstk=Stk}) ->
    {Ra#ra{top=Top+1,maxstk=max(Top+1,Stk)}, [bif0(self,Top)]};

expr({call_remote,Arity,{Mod,Fun,Arity}}, Ra) ->
    Is1 = arg_list(Arity, Ra),
    Ra1 = pop(Arity, Ra),
    {Ra2,Is2} = push({x,0},Ra1),
    Ext = {extfunc,Mod,Fun,Arity},
    {Ra2,Is1++[{call_ext,Arity,Ext}]++Is2};

expr({enter_remote,Arity,{Mod,Fun,Arity}}, Ra) ->
    Is1 = arg_list(Arity, Ra),
    Ext = {extfunc,Mod,Fun,Arity},
    Dealloc = Ra#ra.argn+Ra#ra.varn+Ra#ra.maxstk,
    {Ra,Is1++[{call_ext_last,Arity,Ext,Dealloc}]};

expr({call_local,Arity,_Local={_Fun,Arity}}, Ra) ->
    Is1 = arg_list(Arity, Ra),
    Ra1 = pop(Arity, Ra),
    {Ra2,Is2} = push({x,0},Ra1),
    {Ra2,Is1++[{call,Arity,_Fun}]++Is2};

expr({enter_local,Arity,_Local={_Fun,Arity}}, Ra) ->
    Is1 = arg_list(Arity, Ra),
    Ra1 = pop(Arity, Ra),
    Dealloc = Ra#ra.argn+Ra#ra.varn+Ra#ra.maxstk,
    {Ra1,Is1++[{call_last,Arity,_Fun,Dealloc}]};

expr({label,L},Ra) -> 
    {Ra, [{label,L}]};
expr({goto,{j,L}},Ra) -> 
    {Ra, [{jump,{j,L}}]};
expr({gotoix,Low,JTab},Ra=#ra{fail=Fail}) -> 
    {pop(Ra),
     [{select_val,top(Ra),Fail,
       lists:flatten([[{integer,Low+I-1},element(I,JTab)] ||
			 I <- lists:seq(1,tuple_size(JTab))])}]};

expr(failIf, Ra) ->
    {Ra, [ifEnd]};
expr(failCase, Ra) ->
    {Ra, [{case_end,top(Ra)}]};

expr({Op,L,R},Ra) when is_atom(Op) ->
    {Ra1,Is1} = expr(L,Ra),
    {Ra2,Is2} = expr(R,Ra1),
    {pop(Ra2),Is1++Is2++
	 [{bif,Op,{live,Ra#ra.argn},snd(Ra),top(Ra),snd(Ra)}]}.

expr_list([Expr|ExprList],Ra) ->
    {Ra1,Is1} = expr(Expr, Ra),
    {Ra2,Is2} = expr_list(ExprList, Ra1),
    {Ra2,Is1++Is2};
expr_list([],Ra) ->
    {Ra,[]}.

%% take argument from stack [{x,n-1},..{x,1},{x,0} | _]
%% that is {move,{y,top-n-1},{x,0}},{move,{y,top-n-2},{x,1}},...
arg_list(A, Ra) ->
    [{move,stk(A-I-1,Ra),{x,I}} || I <- lists:seq(0, A-1)].

%% eqArg & eqVar in guard or fail
eq(Reg, Ra) ->
    {pop(Ra), [{is_eq,Ra#ra.fail,Reg,top(Ra)}]}.

push(Src, Ra=#ra{top=Top, maxstk=Stk}) ->
    Ra1 = Ra#ra{top=Top+1, maxstk=max(Stk,Top+1)},
    move(Src, top(Ra1), Ra1).

pop(Ra) -> pop(1, Ra).
pop(N, Ra=#ra{top=Top}) -> Ra#ra{top=Top-N}.

store(Dst, Ra) ->
    move(top(Ra), Dst, pop(Ra)).

compare(Cmp, Ra=#ra{fail=Fail}) ->
    Ra1 = pop(Ra),
    case Cmp of
	comp_lt -> 
	    {Ra1, [{is_lt,Fail,snd(Ra),top(Ra)}]};
	comp_geq -> 
	    {Ra1, [{is_ge,Fail,snd(Ra),top(Ra)}]};
	comp_eqeq -> 
	    {Ra1, [{is_eq,Fail,snd(Ra),top(Ra)}]};
	comp_neq -> 
	    {Ra1, [{is_ne,Fail,snd(Ra),top(Ra)}]};
	comp_gt -> 
	    {Ra1, [{is_lt,Fail,snd(Ra),top(Ra)}]};
	comp_leq -> 
	    {Ra1, [{is_ge,Fail,snd(Ra),top(Ra)}]}
    end.
    
exact(Exact, Ra=#ra{fail=Fail}) ->
    Ra1 = pop(Ra),
    case Exact of
	exact_eqeq ->
	    {Ra1, [{is_eq_exact,Fail,snd(Ra),top(Ra)}]};
	exact_neq ->
	    {Ra1, [{is_ne_exact,Fail,snd(Ra),top(Ra)}]}
    end.

test(Test, Ra) ->
    Ra1 = pop(Ra),
    case Test of
	test_integer -> {Ra1,[{is_integer,Ra#ra.fail,top(Ra)}]};
	test_float -> {Ra1,[{is_float,Ra#ra.fail,top(Ra)}]};
	test_number -> {Ra1,[{is_number,Ra#ra.fail,top(Ra)}]};
	test_atom -> {Ra1,[{is_atom,Ra#ra.fail,top(Ra)}]};
	test_pid -> {Ra1,[{is_pid,Ra#ra.fail,top(Ra)}]};
	test_port -> {Ra1,[{is_port,Ra#ra.fail,top(Ra)}]};
	test_reference -> {Ra1,[{is_reference,Ra#ra.fail,top(Ra)}]};
	test_binary -> {Ra1,[{is_binary,Ra#ra.fail,top(Ra)}]};
	test_constant ->
	    {Ra1,[{is_number,Ra#ra.fail,top(Ra)},
		  {is_binary,Ra#ra.fail,top(Ra)},
		  {is_bitstr,Ra#ra.fail,top(Ra)},
		  {is_atom,Ra#ra.fail,top(Ra)},
		  {is_reference,Ra#ra.fail,top(Ra)},
		  {is_port,Ra#ra.fail,top(Ra)},
		  {is_pid,Ra#ra.fail,top(Ra)}]};
	test_list -> {Ra1,[{is_list,Ra#ra.fail,top(Ra)}]};
	test_tuple -> {Ra1,[{is_tuple,Ra#ra.fail,top(Ra)}]}
    end.

arith1(Arith, Ra=#ra{fail=Fail,argn=Live}) ->
    %% we could scratch x registers here since they are move into y registers
    %% in the prelude, but we may want to optimise the code soon so save them
    Ra1 = Ra,
    case Arith of
	arith_bnot -> {Ra1,[gc_bif1('bnot',Fail,Live,Ra)]};
	arith_neg -> {Ra1,[gc_bif1('-',Fail,Live,Ra)]}
    end.

arith2(Arith, Ra=#ra{fail=Fail,argn=Live}) ->
    Ra1 = pop(Ra),
    case Arith of
	arith_plus -> {Ra1,[gc_bif2('+',Fail,Live,Ra)]};
	arith_minus -> {Ra1,[gc_bif2('-',Fail,Live,Ra)]};
	arith_times -> {Ra1,[gc_bif2('*',Fail,Live,Ra)]};
	arith_div -> {Ra1,[gc_bif2('/',Fail,Live,Ra)]};
	arith_intdiv -> {Ra1,[gc_bif2('div',Fail,Live,Ra)]};
	arith_band -> {Ra1,[gc_bif2('band',Fail,Live,Ra)]};
	arith_bor -> {Ra1,[gc_bif2('bor',Fail,Live,Ra)]};
	arith_bxor -> {Ra1,[gc_bif2('bxor',Fail,Live,Ra)]};
	arith_bsl -> {Ra1,[gc_bif2('bsl',Fail,Live,Ra)]};
	arith_bsr -> {Ra1,[gc_bif2('bsr',Fail,Live,Ra)]};
	arith_rem -> {Ra1,[gc_bif2('rem',Fail,Live,Ra)]}
    end.

gc_bif1(Op,Fail,Live,Ra) ->
    {gc_bif1,Fail,Live,{extfunc,erlang,Op,1},top(Ra),top(Ra)}.

gc_bif2(Op,Fail,Live,Ra) ->
    {gc_bif2,Fail,Live,{extfunc,erlang,Op,2},snd(Ra),top(Ra),snd(Ra)}.

bif0(Op,Ra) ->
    {bif0,{extfunc,erlang,Op,0},top(Ra)}.

bif1(Op,Fail,Ra) ->
    {bif1,Fail,{extfunc,erlang,Op,1},top(Ra),top(Ra)}.

move(Xi, Xi, Ra) ->
    {Ra, []};
move(A, B, Ra) ->
    {Ra, [{move,A,B}]}.

