%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Generate beam code JIT
%%% @end
%%% Created :  4 Jun 2022 by Tony Rogvall <tony@rogvall.se>

-module(beamjit).

-define(verbose(F,A), io:format((F),(A))).
%%-define(verbose(F,A), ok).

-export([jit/1]).
-export([jbegin/0]).
-export([jend/0]).
-export([new_label/0]).
-export([live/1, live/0]).
-export([bind/1]).       %% = label/1
%% aliases using simple assembly names gc_bif +,-,* etc
-export([add/3, sub/3, mul/3]).
-export([module/1, source/1]).
-export([exported_function/4, local_function/4]).
-export([build_module/0]).

%% variants using live/0
-export([bif3/6]).
-export([allocate/1]).
-export([allocate_heap/2]).
-export([allocate_zero/1]).
-export([allocate_heap_zero/2]).
-export([test_heap/1]).
-export([bs_create_bin/5]).

%% the emitting opcodes 
-export([label/1]).              %% 1
-export([func_info/3]).          %% 2
-export([int_code_end/0]).       %% 3
-export([call/2]).               %% 4
-export([call_last/3]).          %% 5
-export([call_only/2]).          %% 6
-export([call_ext/2]).           %% 7
-export([call_ext_last/3]).      %% 8
-export([bif0/2]).               %% 9
-export([bif1/4]).               %% 10
-export([bif2/5]).               %% 11
-export([allocate/2]).           %% 12
-export([allocate_heap/3]).      %% 13
-export([allocate_zero/2]).      %% 14   (not generated in OTP 24?)
-export([allocate_heap_zero/3]). %% 15   (not generated in OTP 24?)
-export([test_heap/2]).          %% 16
-export([init/1]).               %% 17   (not generated in OTP 24?)
-export([deallocate/1]).         %% 18
-export([return/0]).             %% 19
-export([send/0]).               %% 20
-export([remove_message/0]).     %% 21
-export([timeout/0]).            %% 22
-export([loop_rec/2]).           %% 23
-export([loop_rec_end/1]).       %% 24
-export([wait/1]).               %% 25
-export([wait_timeout/2]).       %% 26
-export([m_plus/4]).             %% 27*
-export([m_minus/4]).            %% 28*
-export([m_times/4]).            %% 29*
-export([m_div/4]).              %% 30*
-export([int_div/4]).            %% 31*
-export([int_rem/4]).            %% 32*
-export([int_band/4]).           %% 33*
-export([int_bor/4]).            %% 34*
-export([int_bxor/4]).           %% 35*
-export([int_bsl/4]).            %% 36*
-export([int_bsr/4]).            %% 37*
-export([int_bnot/3]).           %% 38*
-export([is_lt/3]).              %% 39
-export([is_ge/3]).              %% 40
-export([is_eq/3]).              %% 41
-export([is_ne/3]).              %% 42
-export([is_eq_exact/3]).        %% 43
-export([is_ne_exact/3]).        %% 44
-export([is_integer/2]).         %% 45
-export([is_float/2]).           %% 46
-export([is_number/2]).          %% 47
-export([is_atom/2]).            %% 48
-export([is_pid/2]).             %% 49
-export([is_reference/2]).       %% 50
-export([is_port/2]).            %% 51
-export([is_nil/2]).             %% 52
-export([is_binary/2]).          %% 53
-export([is_constant/2]).        %% 54*
-export([is_list/2]).            %% 55
-export([is_nonempty_list/2]).   %% 56
-export([is_tuple/2]).           %% 57
-export([test_arity/3]).         %% 58
-export([select_val/3]).         %% 59
-export([select_tuple_arity/3]). %% 60
-export([jump/1]).               %% 61
-export(['catch'/2]).            %% 62
-export([catch_end/1]).          %% 63
-export([move/2]).               %% 64
-export([get_list/3]).           %% 65
-export([get_tuple_element/3]).  %% 66
-export([set_tuple_element/3]).  %% 67
-export([put_string/3]).         %% 68*
-export([put_list/3]).           %% 69
-export([put_tuple/2]).          %% 70
-export([put/1]).                %% 71
-export([badmatch/1]).           %% 72
-export([if_end/0]).             %% 73
-export([case_end/1]).           %% 74
-export([call_fun/1]).           %% 75
-export([make_fun/3]).           %% 76*
-export([is_function/2]).        %% 77
-export([call_ext_only/2]).      %% 78
-export([bs_start_match/2]).     %% 79*
-export([bs_get_integer/2]).     %% 80*
-export([bs_get_float/2]).       %% 81*
-export([bs_get_binary/2]).      %% 82*
-export([bs_skip_bits/2]).       %% 83*
-export([bs_test_tail/2]).       %% 84*
-export([bs_save/1]).            %% 85*
-export([bs_restore/1]).         %% 86*
-export([bs_init/2]).            %% 87*
-export([bs_final/2]).           %% 88*
-export([bs_put_integer/5]).     %% 89
-export([bs_put_binary/5]).      %% 90
-export([bs_put_float/5]).       %% 91
-export([bs_put_string/2]).      %% 92
-export([bs_need_buf/1]).        %% 93*
-export([fclearerror/0]).        %% 94
-export([fcheckerror/1]).        %% 95
-export([fmove/2]).              %% 96
-export([fconv/2]).              %% 97
-export([fadd/4]).               %% 98
-export([fsub/4]).               %% 99
-export([fmul/4]).               %% 100
-export([fdiv/4]).               %% 101
-export([fnegate/3]).            %% 102
-export([make_fun2/1]).          %% 103*
-export(['try'/2]).              %% 104*
-export([try_end/1]).            %% 105*
-export([try_case/1]).           %% 106*
-export([try_case_end/1]).       %% 107*
-export([raise/2]).              %% 108*
-export([bs_init2/6]).           %% 109
-export([bs_bits_to_bytes/3]).   %% 110*
-export([bs_add/5]).             %% 111
-export([apply/1]).              %% 112
-export([apply_last/2]).         %% 113
-export([is_boolean/2]).         %% 114
-export([is_function2/3]).       %% 115
-export([bs_start_match2/5]).    %% 116*
-export([bs_get_integer2/7]).    %% 117*
-export([bs_get_float2/7]).      %% 118*
-export([bs_get_binary2/7]).     %% 119*
-export([bs_skip_bits2/5]).      %% 120*
-export([bs_test_tail2/3]).      %% 121*
-export([bs_save2/2]).           %% 122*
-export([bs_restore2/2]).        %% 123*
-export([gc_bif1/5]).            %% 124
-export([gc_bif2/6]).            %% 125
-export([bs_final2/2]).          %% 126*
-export([bs_bits_to_bytes2/2]).  %% 127*
-export([put_literal/2]).        %% 128*
-export([is_bitstr/2]).          %% 129
-export([bs_context_to_binary/1]). %% 130
-export([bs_test_unit/3]).       %% 131
-export([bs_match_string/4]).    %% 132
-export([bs_init_writable/0]).   %% 133
-export([bs_append/8]).          %% 134
-export([bs_private_append/6]).  %% 135
-export([trim/2]).               %% 136
-export([bs_init_bits/6]).       %% 137
-export([bs_get_utf8/5]).        %% 138
-export([bs_skip_utf8/4]).       %% 139
-export([bs_get_utf16/5]).       %% 140
-export([bs_skip_utf16/4]).      %% 141
-export([bs_get_utf32/5]).       %% 142
-export([bs_skip_utf32/4]).      %% 143
-export([bs_utf8_size/3]).       %% 144
-export([bs_put_utf8/3]).        %% 145
-export([bs_utf16_size/3]).      %% 146
-export([bs_put_utf16/3]).       %% 147
-export([bs_put_utf32/3]).       %% 148
-export([on_load/0]).            %% 149
-export([recv_mark/1]).          %% 150
-export([recv_set/1]).           %% 151
-export([gc_bif3/7]).            %% 152
-export([line/1]).               %% 153
-export([put_map_assoc/5]).      %% 154*
-export([put_map_exact/5]).      %% 155*
-export([is_map/2]).             %% 156*
-export([has_map_fields/3]).     %% 157*
-export([get_map_elements/3]).   %% 158*
-export([is_tagged_tuple/4]).    %% 159
-export([build_stacktrace/0]).   %% 160
-export([raw_raise/0]).          %% 161
-export([get_hd/2]).             %% 162
-export([get_tl/2]).             %% 163
-export([put_tuple2/2]).         %% 164
-export([bs_get_tail/3]).        %% 165
-export([bs_start_match3/4]).    %% 166
-export([bs_get_position/3]).    %% 167
-export([bs_set_position/2]).    %% 168
-export([swap/2]).               %% 169
-export([bs_start_match4/4]).    %% 170
-export([make_fun3/3]).          %% 171
-export([init_yregs/1]).         %% 172
-export([recv_marker_bind/2]).   %% 173
-export([recv_marker_clear/1]).  %% 174
-export([recv_marker_reserve/1]). %% 175
-export([recv_marker_use/1]).     %% 176
-export([bs_create_bin/6]).       %% 177
-export([call_fun2/3]).           %% 178
-export([nif_start/0]).           %% 179
-export([badrecord/1]).           %% 180

%% debug/test
-export([load_file/1, load_binary/1]).
-export([print_file/1, print_binary/1, print_chunks/1]).
-export([test_file/1, test_binary/1, test_chunks/1]).
-export([obsolete/0, obsolete/1]).
-export([encode/1]).
-export([decode/1]).
-export([is_bif/2, is_gc_bif/2]).

-define(is_byte(X), (((bnot 16#ff) band (X)) =:= 0)).
-define(is_reg(X), (((bnot 16#3ff) band (X)) =:= 0)).  %% 0..1023

-type import() :: {Mod::atom(),Fun::atom(),Arity::integer()}.
-type export() :: {Name::atom(),Arity::integer(),Entry::integer()}.
-type local()  :: {Name::atom(),Arity::integer(),Entry::integer()}.
-type line()   :: {Filename::string(),Lineno::integer()}.

-type jctx_map(T) :: #{ Item::T => Index::integer(),
		        Index::integer() => Item::T }.

-record(jctx_table,
	{
	 index = 0,
	 map = #{}
	}).

-record(jctx,
	{
	 fd,
	 module,
	 source = "",
	 max_op = 0,
	 level = 0,
	 label = 0,
	 func = 0,
	 live = 0,
	 num_lines = 0,
	 atoms     :: #jctx_table{map::jctx_map(atom())},
	 literals  :: #jctx_table{map::jctx_map(term())},
	 imports   :: #jctx_table{map::jctx_map(import())},
	 exports   :: #jctx_table{map::jctx_map(export())},
	 locals    :: #jctx_table{map::jctx_map(local())},
	 lines     :: #jctx_table{map::jctx_map(line())},
	 strings   :: binary()
	}).


-define(U,   2#000).
-define(I,   2#001).
-define(A,   2#010).
-define(X,   2#011).
-define(Y,   2#100).
-define(F,   2#101).
-define(H,   2#110).
-define(Z,   2#111).

-ifdef(PRE_OTP_20).
-define(FLOAT,   1).
-define(LIST,    2).
-define(FR,      3.
-define(ALLOC,   4).
-define(LITERAL, 5).
-else.
-define(LIST,    1).
-define(FR,      2).
-define(ALLOC,   3).
-define(LITERAL, 4).
-define(TR,      5).
-endif.
-define(ALLOC_WORDS,  0).
-define(ALLOC_FLOATS, 1).
-define(ALLOC_FUNS,   2).

-define(BIT_BIG,      16#00).
-define(BIT_LITTLE,   16#02).
-define(BIT_UNSIGNED, 16#00).
-define(BIT_SIGNED,   16#04).
-define(BIT_NATIVE,   16#10).
-define(BIT_EXACT,    16#08).

-define(MAX_OP_CODE, 176).

-type op() :: 1..?MAX_OP_CODE.
-type version() :: integer().  %% Major:16,Minor:8,Patch:8
-type argtype() :: 
	%% tag types
	r |    %% {x,0}
	x |    %% {x,i} register i=0..1023
	y |    %% {y,j} stack slot i=0..1023
	a |    %% atom tag
	i |    %% integer tag
	u |    %% unsigned tag
	n |    %% nil (a 0)
	q |    %% literal term
	k |    %% alloc list
	%% composed
	c   |  %% i|a|n|q
	d   |  %% x|y
	s   |  %% x|y|i|a|n|q
	'S' |  %% x|y
	j   |  %% f     jump label
	'E' |  %% u     export entry
	'L' |  %% u     line info entry
	'A' |  %% u     arity value
	'b' |  %% u     Bif {erlang,Name,Arity}?
	'F' |  %% u     MFA
	'U' |  %% u     unsigned value
	'G' |  %% u     bit flags
	'R' |  %% u ... arg list
	st  |  %% u     string arg {string,binary()} in string table
	h   |  %% u     character
	l   |  %% l     float reg
	dlq |  %% d|l|q
	dl     %% d|l
	.

-type bitflag() :: 
	signed | unsigned |
	big | little |
	native | exact.

-type alloc() ::
	{word, unsigned()} |
	{floats, unsigned()} |
	{funs, unsigned()}.

-type unsigned() :: non_neg_integer().
-type jarg() :: {f,Lbl::unsigned()}.
-type xarg() :: {x,Reg::0..1023}.
-type yarg() :: {y,Reg::0..1023}.
-type iarg() :: {i,integer()}.
-type uarg() :: {u,unsigned()}|unsigned().
-type aarg() :: {atom,atom()}|atom().
-type qarg() :: {literal, term()}.
-type larg() :: {fr,Reg::0..1023}.
-type narg() :: [].
-type carg() :: iarg()|aarg()|narg()|qarg().
-type karg() :: {alloc,[alloc()]}|uarg().
-type reg() :: xarg()|yarg().
-type src() :: reg()|carg().
-type dst() :: reg().
-type dl()  :: dst()|larg().
-type dlq() :: dst()|larg()|qarg().
-type mfa(A) :: {extfunc,Mod::atom(),Func::atom(),A}.
-type linearg() :: [] | [{location,Filename::string(),Lineno::integer()}].
-type listarg() :: {list,[src()]}.
-type fieldflags() :: {field_flags,[bitflag()]}|[bitflag()].

-define(JARG, {f,_}).
	
-record(opcode,
	{
	 mnemonic,
	 op :: op(),
	 arity :: integer(),
	 vsn_introduced :: version(),
	 vsn_deprecated :: version(),
	 argtypes :: [argtype()]
	}).

-define(VSN(X,Y,Z), (((X) bsl 16) bor ((Y) bsl 8) bor (Z))).
-define(OTP_R4,     ?VSN(4,0,0)).
-define(OTP_R5,     ?VSN(5,0,0)).
-define(OTP_R7,     ?VSN(7,0,0)).
-define(OTP_R7A,    ?VSN(7,$A,0)).
-define(OTP_R7B,    ?VSN(7,$B,0)).
-define(OTP_R8,     ?VSN(8,0,0)).
-define(OTP_R10B,   ?VSN(10,$B,0)).
-define(OTP_R10B_6, ?VSN(10,$B,6)).
-define(OTP_R11B,   ?VSN(11,$B,0)).
-define(OTP_R11B_4, ?VSN(11,$B,4)).
-define(OTP_R11B_5, ?VSN(11,$B,5)).
-define(OTP_R12B,   ?VSN(12,$B,0)).
-define(OTP_R12B_5, ?VSN(12,$B,5)).
-define(OTP_R13,    ?VSN(13,0,0)).
-define(OTP_R13B_03,?VSN(13,$B,3)).
-define(OTP_R14A,   ?VSN(14,$A,0)).
-define(OTP_R15A,   ?VSN(15,$A,0)).
-define(OTP_R17,    ?VSN(17,0,0)).
-define(OTP_20,    ?VSN(20,0,0)).
-define(OTP_21,    ?VSN(21,0,0)).
-define(OTP_22,    ?VSN(22,0,0)).
-define(OTP_23,    ?VSN(23,0,0)).
-define(OTP_24,    ?VSN(24,0,0)).
-define(OTP_25,    ?VSN(25,0,0)).

-define(DEFAULT_VSN, ?OTP_R4).

%% @doc Specify a module local label.
%%      Label gives this code address a name Lbl and marks the start of
%%      a basic block.
%%
%%   <h4>opcode: 1</h4>
%% @end
%% @since OTP-R4

-spec label(Lbl::unsigned()) -> ok.
label(Lbl) when is_integer(Lbl), Lbl >= 0 ->
    emit_instruction(label, [Lbl]).

%% @doc Define a function M:F/A.
%%   <h4>opcode: 2</h4>
%% @end
%% @since OTP-R4
-spec func_info(Mod::aarg(),Func::aarg(),Arity::uarg()) -> ok.

func_info(Mod,Func,Arity) when is_atom(Mod), is_atom(Func), 
			       is_integer(Arity), Arity >= 0 ->
    emit_instruction(func_info, [Mod, Func, Arity]);
func_info({atom,Mod},{atom,Func},Arity) when is_atom(Mod), is_atom(Func), 
					     is_integer(Arity), Arity >= 0 ->
    emit_instruction(func_info, [Mod, Func, Arity]).

%% @doc Mark end of module.
%%   <h4>opcode: 3</h4>
%% @end
%% @since OTP-R4
-spec int_code_end() -> ok.

int_code_end() ->
    emit_instruction(int_code_end).

%% @doc Call the function at Label.
%%      Save the next instruction as the return address in the CP register.
%%   <h4>opcode: 4</h4>
%% @end
%% @since OTP-R4
-spec call(Arity::uarg(), Label::jarg()) -> ok.

call(Arity, Label) ->
    emit_instruction(call, [Arity, Label]).

%% @doc Deallocate and do a tail recursive call to the function at Label.
%%      Do not update the CP register.
%%      Before the call deallocate Deallocate words of stack.
%%   <h4>opcode: 5</h4>
%% @end
%% @since OTP-R4
-spec call_last(Arity::uarg(), Label::jarg(), Dealloc::uarg()) -> ok.
call_last(Arity, Label, Dealloc) ->
    emit_instruction(call_last, [Arity,Label,Dealloc]).

%% @doc Do a tail recursive call to the function at Label.
%%      Do not update the CP register.
%%   <h4>opcode: 6</h4>
%% @end
%% @since OTP-R4
call_only(Arity,Label) ->
    emit_instruction(call_only, [Arity, Label]).

%% @doc Call the function of arity Arity pointed to by Destination.
%%      Save the next instruction as the return address in the CP register.
%%   <h4>opcode: 7</h4>
%% @end
%% @since OTP-R4
call_ext(Arity,Ext) ->
    emit_instruction(call_ext, [Arity, Ext]).

%% @doc Deallocate and do a tail call to function of arity Arity
%%      pointed to by Destination.
%%      Do not update the CP register.
%%      Deallocate Deallocate words from the stack before the call.
%%   <h4>opcode: 8</h4>
%% @end
%% @since OTP-R4
call_ext_last(Arity,Ext,Dealloc) ->
    emit_instruction(call_ext_last,[Arity,Ext,Dealloc]).

%% @doc Call the bif Bif and store the result in Reg.
%%   <h4>opcode: 9</h4>
%% @end
-spec bif0(Bif::atom()|mfa(0), Dst::dst()) ->
	  ok.
bif0(ExtFunc={extfunc,erlang,Bif,0}, Dst) when is_atom(Bif) ->
    emit_instruction(bif0,[ExtFunc,Dst]);
bif0(Bif, Dst) when is_atom(Bif) ->
    emit_instruction(bif0,[{extfunc,erlang,Bif,0},Dst]).

%% @doc Call the bif Bif with the argument Arg, and store the result in Reg.
%%      On failure jump to Lbl.
%%   <h4>opcode: 10</h4>
%% @end
%% @since OTP-R4
-spec bif1(Lbl::jarg(), Bif::atom()|mfa(1), Arg::src(), Dst::dst()) ->
	  ok.
bif1(Lbl=?JARG,{extfunc,erlang,Bif,1}, Arg, Dst) when is_atom(Bif) ->
    bif1_(Lbl, Bif, Arg, Dst);
bif1(Lbl=?JARG, Bif, Arg, Dst) when is_atom(Bif) ->
    bif1_(Lbl, Bif, Arg, Dst).

bif1_(Lbl, Bif, Arg, Dst) ->
    case is_bif(Bif, 1) of
	false -> error({Bif, not_a_bif});
	true ->
	    case is_gc_bif(Bif, 1) of
		true ->
		    gc_bif1(Lbl,live(),Bif,Arg,Dst);
		false ->
		    emit_instruction(bif1,[Lbl,{extfunc,erlang,Bif,1},
					   Arg,Dst]) 
	    end
    end.

%% @doc Call the bif Bif with the arguments Arg1 and Arg2,
%%      and store the result in Reg.
%%      On failure jump to Lbl.
%%   <h4>opcode: 11</h4>
%% @end
%% @since OTP-R4
-spec bif2(Lbl::jarg(), Bif::atom()|mfa(2), Arg1::src(), Arg2::src(),
	   Dst::dst()) -> ok.

bif2(Lbl=?JARG,{extfunc,erlang,Bif,2},Arg1,Arg2,Dst) when is_atom(Bif) ->
    bif2_(Lbl,Bif,Arg1,Arg2,Dst);
bif2(Lbl=?JARG, Bif, Arg1, Arg2, Dst) when is_atom(Bif) ->
    bif2_(Lbl, Bif, Arg1, Arg2, Dst).

bif2_(Lbl, Bif, Arg1, Arg2, Dst) ->
    case is_bif(Bif, 2) of
	false -> error({Bif, not_a_bif});
	true ->
	    case is_gc_bif(Bif, 2) of
		true ->
		    gc_bif2(Lbl,live(),Bif,Arg1,Arg2,Dst);
		false ->
		    emit_instruction(bif2,[Lbl,{extfunc,erlang,Bif,2},
					   Arg1,Arg2,Dst]) 
	    end
    end.

-spec bif3(Lbl::jarg(), Bif::atom()|mfa(3),
	   Arg1::src(), Arg2::src(),Arg3::src(),
	   Dst::dst()) -> ok.

bif3(Lbl=?JARG,{extfunc,erlang,Bif,3},Arg1,Arg2,Arg3,Dst)  when is_atom(Bif) ->
    bif3_(Lbl,Bif,Arg1,Arg2,Arg3,Dst);
bif3(Lbl=?JARG, Bif, Arg1, Arg2, Arg3, Dst) when is_atom(Bif) ->
    bif3_(Lbl, Bif, Arg1, Arg2, Arg3, Dst).

bif3_(Lbl, Bif, Arg1, Arg2, Arg3, Dst) ->
    case is_bif(Bif, 3) of
	false -> error({Bif, not_a_bif});
	true ->
	    case is_gc_bif(Bif, 3) of
		true ->
		    gc_bif3(Lbl,live(),Bif,Arg1,Arg2,Arg3,Dst)
		%% there is no BIF3 yet..
	    end
    end.

%% @doc Allocate space for StackNeed words on the stack. If a GC is needed
%%      during allocation there are Live number of live X registers.
%%      Also save the continuation pointer (CP) on the stack.
%%   <h4>opcode: 12</h4>
%% @end
%% @since OTP-R4
-spec allocate(StackNeed::uarg(), Live::uarg()) -> ok.
allocate(StackNeed,Live) ->
    emit_instruction(allocate,[StackNeed,Live]).

%% @doc Like allocate but Live is taken from the last call to
%%           live/1 library function.
%%   <h4>opcode: 12</h4>
%% @end
%% @equiv allocate(StackNeed, live())
%% @since OTP-R4
-spec allocate(StackNeed::uarg()) -> ok.
allocate(StackNeed) ->
    allocate(StackNeed, live()).

%% @doc Allocate space for StackNeed words on the stack and ensure
%%      there is space for HeapNeed words on the heap. If a GC is needed
%%      save Live number of X registers.
%%      Also save the continuation pointer (CP) on the stack.
%%   <h4>opcode: 13</h4>
%% @end
%% @since OTP-R4
-spec allocate_heap(StackNeed::uarg(),HeapNeed::karg(),Live::uarg()) -> ok.
allocate_heap(StackNeed,HeapNeed,Live) ->
    emit_instruction(allocate_heap,[StackNeed,HeapNeed,Live]).

%% @doc Allocate space for Stack and Heap.
%%   <h4>opcode: 13</h4>
%% @end
%% @equiv allocate_heap(StackNeed,HeapNeed,live())
%% @since OTP-R4

-spec allocate_heap(StackNeed::uarg(),HeapNeed::karg()) -> ok.
allocate_heap(StackNeed,HeapNeed) ->
    allocate_heap(StackNeed,HeapNeed,live()).

%% @doc Allocate space for StackNeed words on the stack. If a GC is needed
%%      during allocation there are Live number of live X registers.
%%      Clear the new stack words. (By writing NIL.)
%%      Also save the continuation pointer (CP) on the stack.
%%
%%      OTP 24: This instruction has been superseded by @see allocate/2 followed
%%      by @see init_yregs/1.
%%   <h4>opcode: 14</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-24.


allocate_zero(StackNeed,Live) ->
    emit_instruction(allocate_zero,[StackNeed,Live]).

%% @doc Allocate space for StackNeed words on the stack.
%%   <h4>opcode: 14</h4>
%% @end
%% @equiv allocate_zero(StackNeed,live())
%% @since OTP-R4
allocate_zero(StackNeed) ->
    allocate_zero(StackNeed,live()).

%% @doc Allocate space for StackNeed words on the stack and HeapNeed words
%%      on the heap. If a GC is needed
%%      during allocation there are Live number of live X registers.
%%      Clear the new stack words. (By writing NIL.)
%%      Also save the continuation pointer (CP) on the stack.
%%
%%      OTP 24: This instruction has been superseded by @see allocate_heap/2
%%      followed by @see init_yregs/1.
%%   <h4>opcode: 15</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-24
-spec allocate_heap_zero(StackNeed::unsigned(),HeapNeed::unsigned()) -> ok.
allocate_heap_zero(StackNeed,HeapNeed) ->
    allocate_heap_zero(StackNeed,HeapNeed,live()).
allocate_heap_zero(StackNeed,HeapNeed,Live) ->
    emit_instruction(allocate_heap_zero,[StackNeed,HeapNeed,Live]).

%% @doc Ensure there is space for HeapNeed words on the heap. If a GC is needed
%%      save Live number of X registers.
%%   <h4>opcode: 16</h4>
%% @end
%% @since OTP-R4
-spec test_heap(HeapNeed::unsigned()) -> ok.
test_heap(HeapNeed) ->
    test_heap(HeapNeed,live()).
test_heap(HeapNeed,Live) ->
    emit_instruction(test_heap,[HeapNeed,Live]).

%% @doc Clear the Nth stack word. (By writing NIL.)
%%       OTP 24: This instruction has been superseded by init_yregs/1.    
%%   <h4>opcode: 17</h4>
%% @end
%% @since OTP-R4
-spec init(N::unsigned()) -> ok.
init(N) ->
    emit_instruction(init,[N]).

%% @doc Restore the continuation pointer (CP) from the stack and deallocate
%%       N+1 words from the stack (the + 1 is for the CP).
%%   <h4>opcode: 18</h4>
%% @end
%% @since OTP-R4
-spec deallocate(N::unsigned()) -> ok.
deallocate(N) ->
    emit_instruction(deallocate,[N]).

%% @doc Return to the address in the continuation pointer (CP).
%%   <h4>opcode: 19</h4>
%% @end
return() ->
    emit_instruction(return).

%% @doc Send argument in x(1) as a message to the destination process in x(0).
%%       The message in x(1) ends up as the result of the send in x(0).
%%   <h4>opcode: 20</h4>
%% @end
%% @since OTP-R4
send() ->
    emit_instruction(send).

%% @doc Unlink the current message from the message queue. Remove any timeout.
%%   <h4>opcode: 21</h4>
%% @end
%% @since OTP-R4
remove_message() ->
    emit_instruction(remove_message).

%% @doc Reset the save point of the mailbox and clear the timeout flag.
%%   <h4>opcode: 22</h4>
%% @end
%% @since OTP-R4
timeout() ->
    emit_instruction(timeout).

%% @doc Loop over the message queue, if it is empty jump to Label.
%%   <h4>opcode: 23</h4>
%% @end
%% @since OTP-R4
loop_rec(F,Dst) ->
    emit_instruction(loop_rec,[F,Dst]).

%% @doc Advance the save pointer to the next message and jump back to Label.
%%   <h4>opcode: 24</h4>
%% @end
%% @since OTP-R4
loop_rec_end(F) ->
    emit_instruction(loop_rec_end,[F]).

%% @doc  Suspend the processes and set the entry point to the beginning of the
%%       receive loop at Label.
%%   <h4>opcode: 24</h4>
%% @end
%% @since OTP-R4
wait(F) ->
    emit_instruction(wait,[F]).

%% @doc  Sets up a timeout of Time milliseconds and saves the address of the
%%       following instruction as the entry point if the timeout triggers.
%%   <h4>opcode: 26</h4>
%% @end
%% @since OTP-R4
wait_timeout(F,Src) ->
    emit_instruction(wait_timeout,[F,Src]).

%% @doc Mixed add. add number Arg1 to number Arg and store result in 
%%      register Dst
%%   <h4>opcode: 27</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec m_plus(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

m_plus(Lbl=?JARG,Arg1,Arg2,Reg) -> 
    emit_instruction(m_plus,[Lbl,Arg1,Arg2,Reg]).

%% @doc Mixed subtract. subtract number Arg1 from number Arg2 and store result 
%%      in register Dst register.
%%   <h4>opcode: 28</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec m_minus(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

m_minus(Lbl=?JARG,Arg1,Arg2,Dst) ->
    emit_instruction(m_minus,[Lbl,Arg1,Arg2,Dst]).

%% @doc Mixed multiply. multiply number Arg1 to number Arg2 and store result in 
%%      register Dst
%%   <h4>opcode: 29</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec m_times(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

m_times(Lbl=?JARG,Arg1,Arg2,Reg) ->
    emit_instruction(m_times,[Lbl,Arg1,Arg2,Reg]).

%% @doc Mixed divide. Divide number Arg1 by number Arg2 and store result in 
%%      register Dst
%%   <h4>opcode: 30</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec m_div(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

m_div(Lbl=?JARG,Arg1,Arg2,Reg) ->
    emit_instruction(m_div,[Lbl,Arg1,Arg2,Reg]).

%% @doc Integer divide. Divide integer Arg1 by integer Arg2 and store quotient
%%      in register Dst
%%   <h4>opcode: 31</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec int_div(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

int_div(Lbl=?JARG,Arg1,Arg2,Dst) ->
    emit_instruction(int_div,[Lbl,Arg1,Arg2,Dst]).

%% @doc Integer reminder. Get the integer reminder after integer divition 
%%      with integer Arg1 by integer Arg2 and store result in register Dst
%%   <h4>opcode: 32</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec int_rem(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

int_rem(Lbl=?JARG,Arg1,Arg2,Dst) ->
    emit_instruction(int_rem,[Lbl,Arg1,Arg2,Dst]).

%% @doc Bitwise and. Compute bitwise and of Integer Arg1 and integer Arg2
%%      and store result in register Dst
%%   <h4>opcode: 33</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec int_band(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

int_band(Lbl=?JARG,Arg1,Arg2,Dst) ->
    emit_instruction(int_band,[Lbl,Arg1,Arg2,Dst]).

%% @doc Bitwise or. Compute bitwise or of Integer Arg1 and integer Arg2
%%      and store result in register Dst
%%   <h4>opcode: 34</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec int_bor(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

int_bor(Lbl=?JARG,Arg1,Arg2,Dst) ->
    emit_instruction(int_bor,[Lbl,Arg1,Arg2,Dst]).

%% @doc Bitwise eclusive or. Compute xor of Integer Arg1 and integer Arg2
%%      and store result in register Dst
%%   <h4>opcode: 35</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03

-spec int_bxor(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

int_bxor(Lbl=?JARG,Arg1,Arg2,Dst) ->
    emit_instruction(int_bxor,[Lbl,Arg1,Arg2,Dst]).

%% @doc Shift left. Compute integer shift left of integer Arg1 
%%      of integer Arg2 number of bits
%%      and store result in register Dst
%%   <h4>opcode: 36</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03
-spec int_bsl(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

int_bsl(Lbl=?JARG,Arg1,Arg2,Dst) ->
    emit_instruction(int_bsl,[Lbl,Arg1,Arg2,Dst]).

%% @doc Shift right. Compute integer shift right of integer Arg1 
%%      of integer Arg2 number of bits
%%      and store result in register Dst
%%   <h4>opcode: 37</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03
-spec int_bsr(Lbl::jarg(),Arg1::src(),Arg2::src(),Dst::dst()) -> ok.

int_bsr(Lbl=?JARG,Arg1,Arg2,Dst) ->
    emit_instruction(int_bsr,[Lbl,Arg1,Arg2,Dst]).


%% @doc Bitwise negate. Compute bit invert of integer Arg1 
%%      and store result in register Dst
%%   <h4>opcode: 38</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03
-spec int_bnot(Lbl::jarg(),Arg1::src(),Dst::dst()) -> ok.

int_bnot(Lbl=?JARG,Arg1,Dst) ->
    emit_instruction(int_bnot,[Lbl,Arg1,Dst]).

%% @doc Compare two terms and jump to Lbl if Arg1 is not less than Arg2.
%%   <h4>opcode: 39</h4>
%% @end
is_lt(Lbl=?JARG,Arg1,Arg2) -> 
    emit_instruction(is_lt,[Lbl,Arg1,Arg2]).

%% @doc Compare two terms and jump to Lbl if Arg1 is less than Arg2.
%%   <h4>opcode: 40</h4>
%% @end
is_ge(Lbl=?JARG,Arg1,Arg2) ->
    emit_instruction(is_ge,[Lbl,Arg1,Arg2]).

%% @doc ompare two terms and jump to Lbl if Arg1 is not (numerically) equal to Arg2.
%%   <h4>opcode: 41</h4>
%% @end
is_eq(Lbl,Arg1,Arg2) ->
    emit_instruction(is_eq, [Lbl,Arg1,Arg2]).

%% @doc Compare two terms and jump to Lbl if Arg1 is (numerically) equal to Arg2.
%%   <h4>opcode: 42</h4>
%% @end
is_ne(Lbl,Arg1,Arg2) ->
    emit_instruction(is_ne,[Lbl,Arg1,Arg2]).

%% @doc Compare two terms and jump to Lbl if Arg1 is not exactly equal to Arg2.
%%   <h4>opcode: 43</h4>
%% @end
is_eq_exact(Lbl,Arg1,Arg2) ->
    emit_instruction(is_eq_exact,[Lbl,Arg1,Arg2]).

%% @doc Compare two terms and jump to Lbl if Arg1 is exactly equal to Arg2.
%%   <h4>opcode: 44</h4>
%% @end
is_ne_exact(Lbl,Arg1,Arg2) ->
    emit_instruction(is_ne_exact,[Lbl,Arg1,Arg2]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not an integer.
%%   <h4>opcode: 45</h4>
%% @end
is_integer(Lbl,Arg1) ->
    emit_instruction(is_integer,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a float.
%%   <h4>opcode: 46</h4>
%% @end
is_float(Lbl,Arg1) ->
    emit_instruction(is_float,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a number.
%%   <h4>opcode: 47</h4>
%% @end
is_number(Lbl,Arg1) ->
    emit_instruction(is_number,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not an atom.
%%   <h4>opcode: 48</h4>
%% @end
is_atom(Lbl,Arg1) ->
    emit_instruction(is_atom,[Lbl,Arg1]).    

%% @doc Test the type of Arg1 and jump to Lbl if it is not a pid.
%%   <h4>opcode: 49</h4>
%% @end
is_pid(Lbl,Arg1) ->
    emit_instruction(is_pid,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a reference.
%%   <h4>opcode: 50</h4>
%% @end
is_reference(Lbl,Arg1) ->
    emit_instruction(is_reference,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a port.
%%   <h4>opcode: 51</h4>
%% @end
is_port(Lbl,Arg1) ->
    emit_instruction(is_port,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not nil.
%%   <h4>opcode: 52</h4>
%% @end
is_nil(Lbl,Arg1) ->
    emit_instruction(is_nil,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a binary.
%%   <h4>opcode: 53</h4>
%% @end
is_binary(Lbl,Arg1) ->
    emit_instruction(is_binary,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is constant.
%%   <h4>opcode: 54</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-13B-3

is_constant(Lbl,Arg1) ->
    emit_instruction(is_constant,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a cons or nil.
%%   <h4>opcode: 55</h4>
%% @end
%% @since OTP-R4
is_list(Lbl,Arg1) ->
    emit_instruction(is_list,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a cons.
%%   <h4>opcode: 56</h4>
%% @end
%% @since OTP-R4
is_nonempty_list(Lbl,Arg1) ->
    emit_instruction(is_nonempty_list,[Lbl,Arg1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a tuple.
%%   <h4>opcode: 57</h4>
%% @end
%% @since OTP-R4
is_tuple(Lbl,Arg1) ->
    emit_instruction(is_tuple,[Lbl,Arg1]).

%% @doc Test the arity of (the tuple in) Arg1 and jump
%% to Lbl if it is not equal to Arity.
%%   <h4>opcode: 58</h4>
%% @end
%% @since OTP-R4
test_arity(Lbl,Src,Size) ->
    emit_instruction(test_arity,[Lbl,Src,Size]).

%% @doc Jump to the destination label corresponding to Arg
%%      in the Destinations list, if no arity matches, jump to Lbl.
%%   <h4>opcode: 59</h4>
%% @end
%% @since OTP-R4
select_val(Val,Lbl,Pairs) ->
    emit_instruction(select_val, [Val,Lbl,Pairs]).

%% @doc Check the arity of the tuple Tuple and jump to the corresponding
%%      destination label, if no arity matches, jump to Lbl.
%%   <h4>opcode: 60</h4>
%% @end
%% @since OTP-R4
select_tuple_arity(Val,Lbl,Pairs) ->
    emit_instruction(select_tuple_arity,[Val,Lbl,Pairs]).

%% @doc Jump to Label.
%%   <h4>opcode: 61</h4>
%% @end
%% @since OTP-R4
jump(F=?JARG) ->
    emit_instruction(jump, [F]).

%% @doc Catch. Setup a catch environment in stack position Dst and 
%%  jump to Lbl in case of failure.
%%   <h4>opcode: 62</h4>
%% @end
%% @since OTP-R4
-spec 'catch'(Dst::yarg(), Lbl::jarg()) -> ok.
'catch'(Dst,Lbl) ->
    emit_instruction('catch',[Dst,Lbl]).

%% @doc Catch end. Unwind (current) catch environment in Dst
%%   <h4>opcode: 63</h4>
%% @end
%% @since OTP-R4
-spec catch_end(Dst::yarg()) -> ok.
catch_end(Dst) ->
    emit_instruction(catch_end,[Dst]).

%% @doc Move the source Src (a literal or a register) to
%%      the destination register Dst.
%%   <h4>opcode: 64</h4>
%% @end
%% @since OTP-R4
-spec move(Src::src(), Dst::dst()) -> ok.

move(Src,Dst) ->
    emit_instruction(move,[Src,Dst]).

%% @doc Get the head and tail (or car and cdr) parts of a list
%%       (a cons cell) from Src and put them into the registers
%%       Head and Tail.
%%   <h4>opcode: 65</h4>
%% @end
%% @since OTP-R4

-spec get_list(Src::src(), Head::dst(), Tail::dst()) -> ok.
get_list(Src,Head,Tail) ->
    emit_instruction(get_list,[Src,Head,Tail]).

%% @doc  Get element number Element from the tuple in Source and put
%%       it in the destination register Destination.     
%%   <h4>opcode: 66</h4>
%% @end
%% @since OTP-R4

get_tuple_element(Src,Ix,Dst) ->
    emit_instruction(get_tuple_element,[Src,Ix,Dst]).

%% @doc  Update the element at position Position of the tuple Tuple
%%       with the new element NewElement.
%%   <h4>opcode: 67</h4>
%% @end
%% @since OTP-R4

set_tuple_element(Val,Dst,Ix) ->
    emit_instruction(set_tuple_element,[Val,Dst,Ix]).

put_string(Len,String,Dst) ->
    emit_instruction(put_string,[Len,String,Dst]).

put_list(Head,Tail,Dst) ->
    emit_instruction(put_list,[Head,Tail,Dst]).

put_tuple(Arity,Dst) ->
    emit_instruction(put_tuple,[Arity,Dst]).

put(Src) ->
    emit_instruction(put,[Src]).

badmatch(Lbl) ->
    emit_instruction(badmatch,[Lbl]).

if_end() ->
    emit_instruction(if_end).

case_end(CaseVal) ->
    emit_instruction(case_end,[CaseVal]).

%% @doc Call a fun of arity Arity. Assume arguments in
%%      registers x(0) to x(Arity-1) and that the fun is in x(Arity).
%%      Save the next instruction as the return address in the CP register.
%%   <h4>opcode: 75</h4>
%% @end
%% @since OTP-R4
call_fun(Arity) ->
    emit_instruction(call_fun,[Arity]).

%% @doc Create a fun.
%%   <h4>opcode: 76</h4>
%% @end
%% @since OTP-R4
%% @deprecated OTP-R13B-03
make_fun(Arg1, Arg2, Arg3) ->
    emit_instruction(make_fun,[Arg1, Arg2, Arg3]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a
%%      function (i.e. fun or closure).
%%   <h4>opcode: 77</h4>
%% @end
is_function(Lbl,Arg1) ->
    emit_instruction(is_function,[Lbl,Arg1]).

%% @doc Do a tail recursive call to the function at Label.
%%      Do not update the CP register.
%%   <h4>opcode: 78</h4>
%% @end
%% @since OTP-R5

call_ext_only(Arity,Fun) ->
    emit_instruction(call_ext_only,[Arity,Fun]).

%% @doc
%%   <h4>opcode: 79</h4>
%% @end
%% @since OTP-R7
%% @deprecated OTP-R13B-03
bs_start_match(Lbl,Reg) ->
    emit_instruction(bs_start_match,[Lbl,Reg]).
    
%% @doc
%%   <h4>opcode: 80</h4>
%% @end
%% @since OTP-R7
%% @deprecated OTP-R13B-03
bs_get_integer(Lbl,List) ->
    emit_instruction(bs_get_integer,[Lbl,List]).

%% @doc
%%   <h4>opcode: 81</h4>
%% @end
%% @since OTP-R7
%% @deprecated OTP-R13B-03    
bs_get_float(Lbl,List) ->
    emit_instruction(bs_get_float,[Lbl,List]).

%% @doc
%%   <h4>opcode: 82</h4>
%% @end
%% @since OTP-R7
%% @deprecated OTP-R13B-03    
bs_get_binary(Lbl,List) ->
    emit_instruction(bs_get_binary,[Lbl,List]).

%% @doc
%%   <h4>opcode: 83</h4>
%% @end
%% @since OTP-R7
%% @deprecated OTP-R13B-03
bs_skip_bits(Lbl,List) ->
    emit_instruction(bs_skip_bits,[Lbl,List]).

%% @doc
%%   <h4>opcode: 84</h4>
%% @end
%% @since OTP-R7
%% @deprecated OTP-R13B-03
bs_test_tail(Lbl,List) ->
    emit_instruction(bs_test_tail,[Lbl,List]).

%% @doc
%%   <h4>opcode: 85</h4>
%% @end
%% @since OTP-R7
%% @deprecated OTP-R13B-03
bs_save(N) ->
    emit_instruction(bs_save,[N]).

%% @doc
%%   <h4>opcode: 86</h4>
%% @end
%% @since OTP-R7
%% @deprecated OTP-R13B-03
bs_restore(N) ->
    emit_instruction(bs_restore,[N]).
    
%% @doc
%%   <h4>opcode: 87</h4>
%% @end
%% @since OTP-R7A
%% @deprecated OTP-R13B-03
bs_init(N,Flags) ->
    emit_instruction(bs_init,[N,Flags]).

%% @doc
%%   <h4>opcode: 88</h4>
%% @end
%% @since OTP-R7A
%% @deprecated OTP-R13B-03
bs_final(Lbl,X) ->
    emit_instruction(bs_final,[Lbl,X]).

-spec bs_put_integer(Fail::jarg(),Size::src(),Unit::uarg(),
		     Flags::fieldflags(),ArgInt::src()) -> ok.
bs_put_integer(Fail=?JARG,Size,Unit,Flags,Arg) ->
    emit_instruction(bs_put_integer,[Fail,Size,Unit,Flags,Arg]).

-spec bs_put_binary(Fail::jarg(),Size::src()|{atom,all},
		    Unit::uarg(),Flags::fieldflags(),Arg::src()) -> ok.    
bs_put_binary(Fail=?JARG,Size,Unit,Flags,Arg) ->
    emit_instruction(bs_put_binary,[Fail,Size,Unit,Flags,Arg]).

-spec bs_put_float(Fail::jarg(),Size::src(),
		   Unit::uarg(),Flags::fieldflags(),Arg::src()) -> ok.    
bs_put_float(Fail=?JARG,Size,Unit,Flags,Arg) ->
    emit_instruction(bs_put_float,[Fail,Size,Unit,Flags,Arg]).

bs_put_string(Len,StrArg) ->
    emit_instruction(bs_put_string,[Len,StrArg]).

bs_need_buf(N) ->
    emit_instruction(bs_need_buf,[N]).

%% @doc
%%   <h4>opcode: 94</h4>
%% @end
%% @since OTP-R8
%% @deprecated OTP-24.
-spec fclearerror() -> ok.
fclearerror() ->
    emit_instruction(fclearerror).

%% @doc
%%   <h4>opcode: 95</h4>
%% @end
%% @since OTP-R8
%% @deprecated OTP-24.
-spec fcheckerror(Lbl::jarg()) -> ok.
fcheckerror(Lbl) ->
    emit_instruction(fcheckerror,[Lbl]).


-spec fmove(Src::dlq(), Dst::dl()) -> ok.
fmove(Src,Dst) ->
    emit_instruction(fmove,[Src,Dst]).

-spec fconv(Src::reg(), FDst::larg()) -> ok.
fconv(Src,FDst) ->
    emit_instruction(fconv,[Src,FDst]).

-spec fadd(Fail::jarg(),FArg1::larg(),FArg2::larg(),FDst::larg()) -> ok.
fadd(Fail,FArg1,FArg2,FDst) ->
    emit_instruction(fadd,[Fail,FArg1,FArg2,FDst]).

-spec fsub(Fail::jarg(),FArg1::larg(),FArg2::larg(),FDst::larg()) -> ok.
fsub(Fail,FArg1,FArg2,FDst) ->
    emit_instruction(fsub,[Fail,FArg1,FArg2,FDst]).

-spec fmul(Fail::jarg(),FArg1::larg(),FArg2::larg(),FDst::larg()) -> ok.
fmul(Fail,FArg1,FArg2,FDst) ->
    emit_instruction(fmul,[Fail,FArg1,FArg2,FDst]).

-spec fdiv(Fail::jarg(),FArg1::larg(),FArg2::larg(),FDst::larg()) -> ok.
fdiv(Fail,FA1,FA2,FDst) ->
    emit_instruction(fdiv,[Fail,FA1,FA2,FDst]).

-spec fnegate(Fail::jarg(),FArg1::larg(),FDst::larg()) -> ok.
fnegate(Fail,FArg1,FDst) ->
    emit_instruction(fnegate,[Fail,FArg1,FDst]).

make_fun2(Arg) ->
    emit_instruction(make_fun2,[Arg]).

'try'(Reg,Fail) ->
    emit_instruction('try',[Reg,Fail]).

try_end(Reg) ->
    emit_instruction(try_end,[Reg]).

try_case(Reg) ->
    emit_instruction(try_case,[Reg]).

try_case_end(TryVal) ->
    emit_instruction(try_case_end,[TryVal]).

raise(Class,Reason) ->
    emit_instruction(raise,[Class,Reason]).

bs_init2(Fail,Src,W,R,Flags,Dst) ->
    emit_instruction(bs_init2,[Fail,Src,W,R,Flags,Dst]).

bs_bits_to_bytes(Fail,Src,Dst) ->
    emit_instruction(bs_bits_to_bytes,[Fail,Src,Dst]).

bs_add(Fail,Src1,Src2,Unit,Dst) ->
    emit_instruction(bs_add,[Fail,Src1,Src2,Unit,Dst]).

apply(Arity) ->
    emit_instruction(apply,[Arity]).

apply_last(Arity,U) ->
    emit_instruction(apply_last,[Arity,U]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a Boolean.
is_boolean(Fail,A1) ->
    emit_instruction(is_boolean, [Fail, A1]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a
%%      function of arity Arity.
is_function2(Fail,A1,A2) ->
    emit_instruction(is_function2,[Fail,A1,A2]).

-spec bs_start_match2(Fail::jarg(),Ctx::src(),Live::uarg(),Max::uarg(),Dst::dst()) -> ok.
bs_start_match2(Fail=?JARG,Ctx,Live,Max,Dst) ->
    emit_instruction(bs_start_match2,[Fail,Ctx,Live,Max,Dst]).

-spec bs_get_integer2(Fail::jarg(),Ctx::src(),Live::uarg(),Size::src(),
		      Unit::uarg(),Flags::fieldflags(),Dst::dst()) -> ok.
bs_get_integer2(Fail,Ctx,Live,Size,N,Flags,Dst) ->
    emit_instruction(bs_get_integer2,[Fail,Ctx,Live,Size,N,Flags,Dst]).

bs_get_float2(Fail,Ctx,Live,Size,N,Flags,Dst) ->
    emit_instruction(bs_get_float2,[Fail,Ctx,Live,Size,N,Flags,Dst]).

bs_get_binary2(Fail,Ctx,Live,Size,N,Flags,Dst) ->
    emit_instruction(bs_get_binary2,[Fail,Ctx,Live,Size,N,Flags,Dst]).

-spec bs_skip_bits2(Fail::jarg(),Ctx::src(),Size::src(),Unit::uarg(),
		    Flags::fieldflags()) -> ok.
bs_skip_bits2(Fail=?JARG,Ctx,Size,Unit,Flags) ->
    emit_instruction(bs_skip_bits2,[Fail,Ctx,Size,Unit,Flags]).

bs_test_tail2(Fail,Ctx,N) ->
    emit_instruction(bs_test_tail2,[Fail,Ctx,N]).

bs_save2(Ctx, N) ->
    emit_instruction(bs_save2,[Ctx, N]).

bs_restore2(Ctx, N) ->
    emit_instruction(bs_restore2,[Ctx, N]).

%% @doc Call the bif Bif with the argument Arg, and store the result in Reg.
%%      On failure jump to Lbl.
%%      Do a garbage collection if necessary to allocate space on the heap
%%      for the result (saving Live number of X registers).
-spec gc_bif1(Fail::jarg(),Live::uarg(),Bif::mfa(1)|atom(),
	      Arg::src(),Dst::dst()) -> ok.
gc_bif1(Fail,Live,ExtFunc={extfunc,erlang,_Bif,1},Arg,Dst) ->
    emit_instruction(gc_bif1,[Fail,Live,ExtFunc,Arg,Dst]);
gc_bif1(Fail,Live,Bif,Arg,Dst) when is_atom(Bif) ->
    emit_instruction(gc_bif1,[Fail,Live,{extfunc,erlang,Bif,1},Arg,Dst]).

%% @doc Call the bif Bif with the arguments Arg1 and Arg2,
%%      and store the result in Reg.
%%      On failure jump to Lbl.
%%      Do a garbage collection if necessary to allocate space on the heap
%%      for the result (saving Live number of X registers).
-spec gc_bif2(Fail::jarg(),Live::uarg(),Bif::mfa(2)|atom(),
	      Arg1::src(),Arg2::src(),Dst::dst()) -> ok.
gc_bif2(Fail,Live,ExtFunc={extfunc,erlang,_Bif,2},Arg1,Arg2,Dst) ->
    emit_instruction(gc_bif2,[Fail,Live,ExtFunc,Arg1,Arg2,Dst]);
gc_bif2(Fail,Live,Bif,Arg1,Arg2,Dst) when is_atom(Bif) ->
    emit_instruction(gc_bif2,[Fail,Live,{extfunc,erlang,Bif,2},Arg1,Arg2,Dst]).

%% @doc
%%   <h4>opcode: 126</h4>
%% @end
%% @since OTP-R11B
%% @deprecated OTP-R12B
bs_final2(X,Y) ->
    emit_instruction(bs_final,[X,Y]).

bs_bits_to_bytes2(A2,A3) ->
    emit_instruction(bs_bits_to_bytes2,[A2,A3]).

put_literal(Index, Dst) ->
    emit_instruction(put_literal,[Index, Dst]).

%% @doc Test the type of Arg1 and jump to Lbl if it is not a bit string.
is_bitstr(Fail,A1) ->
    emit_instruction(is_bitstr,[Fail,A1]).

bs_context_to_binary(Dst) ->
    emit_instruction(bs_context_to_binary,[Dst]).

bs_test_unit(Fail,Ctx,N) ->
    emit_instruction(bs_test_unit,[Fail,Ctx,N]).

bs_match_string(Fail,Ctx,Bits,String) ->
    emit_instruction(bs_match_string,[Fail,Ctx,Bits,String]).

bs_init_writable() ->
    emit_instruction(bs_init_writable).
    
bs_append(Fail,Arg2,W,R,U,Arg6,Flags,Arg8) ->
    emit_instruction(bs_append,[Fail,Arg2,W,R,U,Arg6,Flags,Arg8]).

bs_private_append(Fail,Arg2,U,Arg4,Flags,Arg6) ->
    emit_instruction(bs_private_append,[Fail,Arg2,U,Arg4,Flags,Arg6]).

%% @doc Reduce the stack usage by N words,
%%      keeping the CP on the top of the stack.
trim(N,Remaining) ->
    emit_instruction(trim,[N,Remaining]).

bs_init_bits(Fail,Arg2,W,R,Flags,Arg6) ->
    emit_instruction(bs_init_bits,[Fail,Arg2,W,R,Flags,Arg6]).
    
bs_get_utf8(Fail,Arg2,Arg3,Flags,Arg4) ->
    emit_instruction(bs_get_utf8,[Fail,Arg2,Arg3,Flags,Arg4]).

bs_skip_utf8(Fail,Arg2,Arg3,Flags) ->
    emit_instruction(bs_skip_utf8,[Fail,Arg2,Arg3,Flags]).

bs_get_utf16(Fail,Arg2,Arg3,Flags,Arg4) ->
    emit_instruction(bs_get_utf16,[Fail,Arg2,Arg3,Flags,Arg4]).

bs_skip_utf16(Fail,Arg2,Arg3,Flags) ->
    emit_instruction(bs_skip_utf16,[Fail,Arg2,Arg3,Flags]).

bs_get_utf32(Fail,Arg2,Arg3,Flags,Arg4) ->
    emit_instruction(bs_get_utf32,[Fail,Arg2,Arg3,Flags,Arg4]).

bs_skip_utf32(Fail,Arg2,Arg3,Flags) ->
    emit_instruction(bs_skip_utf32,[Fail,Arg2,Arg3,Flags]).

bs_utf8_size(Fail,Arg2,Arg3) ->
    emit_instruction(bs_utf8_size,[Fail,Arg2,Arg3]).

-spec bs_put_utf8(Fail::jarg(),Flags::fieldflags(),Src::src()) -> ok.
bs_put_utf8(Fail,Flags,Src) ->
    emit_instruction(bs_put_utf8,[Fail,Flags,Src]).

bs_utf16_size(Fail,Arg2,Arg3) ->
    emit_instruction(bs_utf16_size,[Fail,Arg2,Arg3]).

-spec bs_put_utf16(Fail::jarg(),Flags::fieldflags(),Src::src()) -> ok.
bs_put_utf16(Fail,Flags,Src) ->
    emit_instruction(bs_put_utf16,[Fail,Flags,Src]).

-spec bs_put_utf32(Fail::jarg(),Flags::fieldflags(),Src::src()) -> ok.
bs_put_utf32(Fail,Flags,Src) ->
    emit_instruction(bs_put_utf32,[Fail,Flags,Src]).

-spec on_load() -> ok.
on_load() ->
    emit_instruction(on_load).

%% @doc  Save the end of the message queue and the address of
%%       the label Label so that a recv_set instruction can start
%%       scanning the inbox from this position.
%% @end
-spec recv_mark(Lbl::jarg()) -> ok.
recv_mark(Lbl) ->
    emit_instruction(recv_mark,[Lbl]).

%% @doc Check that the saved mark points to Label and set the
%%      save pointer in the message queue to the last position
%%      of the message queue saved by the recv_mark instruction.
%% @end
-spec recv_set(Lbl::jarg()) -> ok.
recv_set(Lbl) ->
    emit_instruction(recv_set,[Lbl]).

%% @doc Call the bif Bif with the arguments Arg1, Arg2 and Arg3,
%%      and store the result in Reg.
%%      On failure jump to Lbl.
%%      Do a garbage collection if necessary to allocate space on the heap
%%      for the result (saving Live number of X registers).
%% @end
-spec gc_bif3(Fail::jarg(),Live::uarg(),Bif::mfa(3)|atom(),
	      Arg1::src(),Arg2::src(),Arg3::src(),Dst::dst()) -> ok.
gc_bif3(Fail,Live,ExtFunc={extfunc,erlang,_Bif,3},Arg1,Arg2,Arg3,Dst) ->
    emit_instruction(gc_bif3,[Fail,Live,ExtFunc,Arg1,Arg2,Arg3,Dst]);
gc_bif3(Fail,Live,Bif,Arg1,Arg2,Arg3,Dst) when is_atom(Bif) ->
    emit_instruction(gc_bif3,[Fail,Live,{extfunc,erlang,Bif,3},
			      Arg1,Arg2,Arg3,Dst]).

-spec line(Line::linearg()) -> ok.
line(Line) ->
    inc_num_lines(),
    emit_instruction(line,[Line]).

-spec put_map_assoc(Fail::jarg(),Src::src(),Dst::dst(),
		    N::uarg(),List::listarg()) -> ok.
put_map_assoc(Fail=?JARG,Src,Dst,N,List) ->
    emit_instruction(put_map_assoc,[Fail,Src,Dst,N,List]).

-spec put_map_exact(Fail::jarg(),Src::src(),Dst::dst(),
		    N::uarg(),List::listarg()) -> ok.
put_map_exact(Fail=?JARG,Src,Dst,N,List) ->
    emit_instruction(put_map_exact,[Fail,Src,Dst,N,List]).

is_map(A1,A2) ->
    emit_instruction(is_map,[A1,A2]).

has_map_fields(A1,A2,A3) ->
    emit_instruction(has_map_fields,[A1,A2,A3]).

get_map_elements(A1,A2,A3) ->
    emit_instruction(get_map_elements,[A1,A2,A3]).

%% @doc Test the type of Reg and jumps to Lbl if it is not a tuple.
%%      Test the arity of Reg and jumps to Lbl if it is not N.
%%      Test the first element of the tuple and jumps to Lbl if it is not Atom.
%% @end
is_tagged_tuple(A1,A2,A3,A4) ->
    emit_instruction(is_tagged_tuple,[A1,A2,A3,A4]).

%% @doc  Given the raw stacktrace in x(0), build a cooked stacktrace suitable
%%       for human consumption. Store it in x(0). Destroys all other registers.
%%       Do a garbage collection if necessary to allocate space on the heap
%%       for the result.    
%% @end
build_stacktrace() ->
    emit_instruction(build_stacktrace).

%% @doc  This instruction works like the erlang:raise/3 BIF, except that the
%%       stacktrace in x(2) must be a raw stacktrace.
%%       x(0) is the class of the exception (error, exit, or throw),
%%       x(1) is the exception term, and x(2) is the raw stackframe.
%%       If x(0) is not a valid class, the instruction will not throw an
%%       exception, but store the atom 'badarg' in x(0) and execute the
%%       next instruction.
%% @end
raw_raise() ->
    emit_instruction(raw_raise).

%% @doc  Get the head (or car) part of a list (a cons cell) from Source and
%%       put it into the register Head.
%% @end
get_hd(A1,A2) ->
    emit_instruction(get_hd,[A1,A2]).
    
%% @doc  Get the tail (or cdr) part of a list (a cons cell) from Source and
%%       put it into the register Tail.
%% @end
get_tl(A1,A2) ->
    emit_instruction(get_tl,[A1,A2]).

%% @doc  Build a tuple with the elements in the list Elements and put it
%%       into register Destination.
%% @end
put_tuple2(Dst,List) ->
    emit_instruction(put_tuple2,[Dst,List]).

%% @doc  Sets Dst to the tail of Ctx at the current position
%% @end
-spec bs_get_tail(Ctx::src(), Dst::dst(), Live::unsigned) -> ok.

bs_get_tail(Ctx,Dst,Live) ->
    emit_instruction(bs_get_tail,[Ctx,Dst,Live]).

%% @doc  Starts a binary match sequence
%% @end
bs_start_match3(A1,A2,A3,A4) ->
    emit_instruction(bs_start_match3,[A1,A2,A3,A4]).

%% @doc  Sets Dst to the current position of Ctx
%% @end
bs_get_position(A1,A2,A3) ->
    emit_instruction(bs_get_position,[A1,A2,A3]).

%% @doc  Sets the current position of Ctx to Pos
%% @end
bs_set_position(A1,A2) ->
    emit_instruction(bs_set_position,[A1,A2]).

%% @doc  Swaps the contents of two registers.
%% @end
swap(A1,A2) ->
    emit_instruction(swap,[A1,A2]).

%% @doc  As bs_start_match3, but the fail label can be 'no_fail' when we know
%%       it will never fail at runtime, or 'resume' when we know the input is
%%       a match context.
%% @end
bs_start_match4(A1,A2,A3,A4) ->
    emit_instruction(bs_start_match4,[A1,A2,A3,A4]).

%% @doc  Build a fun with the environment in the list EnvTerms and put it
%%       into register Dst.
%% @end
make_fun3(A1,A2,A3) ->
    emit_instruction(make_fun3,[A1,A2,A3]).
    
%% @doc  Initialize the Y registers in the list.
%% @end
init_yregs(List) ->
    emit_instruction(init_yregs,[List]).

%% @doc  Associates Reference with a previously reserved marker.
%% @end
-spec recv_marker_bind(Marker::src(), Reference::src()) -> ok.
recv_marker_bind(Marker, Reference) ->
    emit_instruction(recv_marker_bind,[Marker, Reference]).

%% @doc  Clears the receive marker associated with the given Reference.
%% @end
-spec recv_marker_clear(Reference::src()) -> ok.
recv_marker_clear(Reference) ->
    emit_instruction(recv_marker_clear,[Reference]).

%% @doc  Creates a receive marker which can be later bound to a reference.
%% @end
-spec recv_marker_reserve(Marker::src()) -> ok.
recv_marker_reserve(Marker) ->
    emit_instruction(recv_marker_reserve,[Marker]).

%% @doc  Sets the current receive cursor to the marker associated with
%%       the given Reference.
%% @end
-spec recv_marker_use(Reference::src()) -> ok.

recv_marker_use(Reference) ->
    emit_instruction(recv_marker_use,[Reference]).

%% @doc  Builda a new binary using the binary syntax.
%% @end
bs_create_bin(Fail,Alloc,Unit,Dst,OpList) ->
    bs_create_bin(Fail,Alloc,live(),Unit,Dst,OpList).
bs_create_bin(Fail,Alloc,Live,Unit,Dst,OpList) ->
    emit_instruction(bs_create_bin,[Fail,Alloc,Live,Unit,Dst,OpList]).

%% @doc Calls the fun Func with arity Arity. Assume arguments in registers x(0)
%%      to x(Arity-1). Tag can be one of:
%%
%%      * FunIndex      - Func is always a local fun identified by FunIndex
%%      * {atom,safe}   - Func is known to be a fun of correct arity.
%%      * {atom,unsafe} - Nothing is known about Func.
%% @end
call_fun2(Tag, Arity, Func) ->
    emit_instruction(call_fun2,[Tag,Arity,Func]).

%% @doc  No-op at start of each function declared in -nifs().
%% @end
-spec nif_start() -> ok.
nif_start() ->
    emit_instruction(nif_start).

%% @doc Raises a {badrecord,Value} error exception.
-spec badrecord(Value::src()) -> ok.
badrecord(Value) ->
    emit_instruction(badrecord,[Value]).

%% Internal

jctx() ->
    get('JCTX').

jctx(Ctx = #jctx {}) ->
    put('JCTX', Ctx).

module(Mod) when is_atom(Mod) ->
    Ctx = jctx(),
    jctx(Ctx#jctx { module = Mod }),
    Mod.

source(Filename) when is_list(Filename) ->
    Ctx = jctx(),
    jctx(Ctx#jctx { source = Filename }),
    Filename.

exported_function(Mod, Name, Arity, Entry) when 
      is_atom(Mod), is_atom(Name), is_integer(Arity), Arity >= 0,
      is_integer(Entry), Entry >= 0 ->
    count_function(),
    _Mi = insert_atom(Mod),
    _Fi = insert_atom(Name),
    insert_export(Name,Arity,Entry),
    func_info(Mod, Name, Arity),
    live(Arity).

local_function(Mod, Name, Arity, Entry) when
      is_atom(Mod), is_atom(Name), is_integer(Arity), Arity >= 0,
      is_integer(Entry), Entry >= 0 ->
    count_function(),
    _Mi = insert_atom(Mod),
    _Fi = insert_atom(Name),
    insert_local(Name,Arity,Entry),
    func_info(Mod, Name, Arity),
    live(Arity).

new_label() ->
    Ctx = jctx(),
    L = Ctx#jctx.label + 1,
    jctx(Ctx#jctx { label = L}),
    L.

count_function() ->
    Ctx = jctx(),
    N = Ctx#jctx.func + 1,
    jctx(Ctx#jctx { func = N }),
    N.    

bind(L) ->
    label(L).

live(Live) when is_integer(Live), Live >= 0, Live < 1024 ->
    Ctx = jctx(),
    jctx(Ctx#jctx { live = Live }),
    Live.

live() ->
    (jctx())#jctx.live.
		
add(Src1, Src2, Dst) ->
    bif2({f,0},'+',Src1, Src2, Dst).

sub(Src1, Src2, Dst) ->
    bif2({f,0}, '-', Src1, Src2, Dst).

mul(Src1, Src2, Dst) ->
    bif2({f,0}, '*', Src1, Src2, Dst).

jit(Fun) ->
    jbegin(),
    try Fun() of
	_ -> jend()
    catch
	error:Reason:Stacktrace ->
	    jend(),
	    erlang:raise(error,Reason,Stacktrace)
    end.

jbegin() ->
    case jctx() of
	undefined -> jinit_();
	0 -> jinit_();
	Ctx -> 
	    Level = Ctx#jctx.level + 1,
	    jctx(Ctx#jctx { level = Level }), ok
    end.

jend() ->
    case jctx() of
	undefined -> error(missing_jbegin);
	Ctx ->
	    case Ctx#jctx.level of
		0 -> error(missing_jbegin);
		1 -> jterminate_();
		L ->
		    Level = L - 1,
		    jctx(Ctx#jctx { level = Level }), ok
	    end
    end.

jinit_() ->
    {ok,Fd} = ram_file:open([], [write,read,binary]),
    Ctx = #jctx {
	     fd = Fd,
	     level = 1,
	     atoms = #jctx_table{index=1},
	     literals = #jctx_table{},
	     imports = #jctx_table{},
	     exports = #jctx_table{},
	     locals = #jctx_table{},
	     lines = #jctx_table{index=1},
	     strings = <<>>
	    },
    jctx(Ctx),
    ok.

jterminate_() ->
    Ctx = jctx(),
    {ok,Bin} = ram_file:get_file(Ctx#jctx.fd),
    ram_file:close(Ctx#jctx.fd),
    erase('JCTX'),
    Bin.

emit_instruction(Mnemomic) ->
    emit_data(encode_instruction(Mnemomic)).
emit_instruction(Mnemomic, Args) ->
    emit_data(encode_instruction(Mnemomic, Args)).

emit_instruction_list(InstructionList) ->
    [emit_instruction(Instr) || Instr <- InstructionList].

encode_instruction(Mnemonic) when is_atom(Mnemonic) ->
    Opcode = maps:get(Mnemonic, opcode_map()),
    encode_instruction_(Opcode, []);
encode_instruction(Instr) when is_tuple(Instr) ->
    [Mnemonic|Args] = tuple_to_list(Instr),
    encode_instruction(Mnemonic,Args).

encode_instruction(Mnemonic,Args) ->
    Opcode = maps:get(Mnemonic, opcode_map()),
    encode_instruction_(Opcode, Args).

encode_instruction_(Opcode, Args) when ?is_byte(Opcode), is_list(Args) ->
    #opcode{mnemonic=_Mnemonic,argtypes=ArgTypes} = 
	maps:get(Opcode, opcode_map()),
    ?verbose("encode_instruction: ~p ~p ~p\n", [_Mnemonic,ArgTypes,Args]),
    [Opcode|encode_arg_list(ArgTypes,Args)].

%% emit_arg_list(ArgTypes, Args) ->
%%     emit_data(encode_arg_list(ArgTypes, Args)).

encode_arg_list(ArgTypes, Args) ->
    encode_arg_list(ArgTypes, Args, [], []).
    
encode_arg_list([ArgType|ArgTypes], [Arg|Args], Stack, Acc) ->
    encode_arg_list(ArgTypes,Args,
		    [Arg|Stack],
		    [encode_arg(ArgType,Arg,Stack)|Acc]);
encode_arg_list([],[],_Stack,Acc) ->
    lists:reverse(Acc).

encode_arg(Type, Arg, Stack) ->
    case Type of
	s -> encode_s(Arg);
	d -> encode_d(Arg);
	u -> encode_u(Arg);
	i -> encode_i(Arg);
	r -> encode_x(Arg);  %% fixme check {x,0}
	x -> encode_x(Arg);
	y -> encode_y(Arg);
	a -> encode_a(Arg);
	j -> encode_j(Arg);
	l -> encode_l(Arg);
	dl -> encode_dl(Arg);
	dlq -> encode_dlq(Arg);
	st -> encode_st(Arg,Stack);
	stb -> encode_stb(Arg,Stack);
	k -> encode_k(Arg);
	'S' -> encode_S(Arg);
	'A' -> encode_A(Arg);
	'F' -> encode_F(Arg);
	'U' -> encode_u(Arg);
	'G' -> encode_G(Arg);
	'L' -> encode_L(Arg);
	'R' -> encode_R(Arg);
	'_' -> encode_(Arg)
    end.

emit_arg(Type, Arg) ->
    case Type of
	s -> emit_s(Arg);
	d -> emit_d(Arg);
	u -> emit_u(Arg);
	i -> emit_i(Arg);
	r -> emit_x(Arg);  %% fixme check {x,0}
	x -> emit_x(Arg);
	y -> emit_y(Arg);
	a -> emit_a(Arg);
	j -> emit_j(Arg);
	l -> emit_l(Arg);
	k -> emit_k(Arg);
	'S' -> emit_S(Arg);
	'A' -> emit_A(Arg);
	'F' -> emit_F(Arg);
	'U' -> emit_u(Arg);
	'G' -> emit_G(Arg);
	'L' -> emit_L(Arg);
	'R' -> emit_R(Arg);
	'_' -> emit_(Arg)
    end.

%% emit_arglist([A|As], [Arg|Args]) ->
%%    emit_arg(A, Arg),
%%    emit_arglist(As, Args);
%% emit_arglist([], []) ->
%%    ok.

%% emit_arg(Type, Arg) ->
%%    emit_data(encode_arg(Type,Arg)).

emit_s(S) -> emit_data(encode_s(S)).
emit_d(D) -> emit_data(encode_d(D)).
emit_u(U) -> emit_data(encode_u(U)).
emit_i(I) -> emit_data(encode_i(I)).
emit_a(A) -> emit_data(encode_a(A)).
emit_x(X) -> emit_data(encode_x(X)).
emit_y(Y) -> emit_data(encode_y(Y)).
emit_j(L) -> emit_data(encode_j(L)).
emit_l(FR) -> emit_data(encode_l(FR)).
emit_k(L)   -> emit_data(encode_k(L)).
emit_S(S) -> emit_data(encode_S(S)).
emit_F(MFA)  -> emit_data(encode_F(MFA)).
emit_A(A) -> emit_data(encode_A(A)).
emit_G(A) -> emit_data(encode_G(A)).
emit_L(Line) -> emit_data(encode_L(Line)).
emit_R(List) -> emit_data(encode_R(List)).    

%% s = x|y|literal
encode_s({x,X}) when ?is_reg(X) ->   encode_ival(?X,X);
encode_s({y,Y})  when ?is_reg(Y) ->  encode_ival(?Y,Y);
encode_s({atom,A}) -> encode_atom(A);
encode_s({integer,I}) -> encode_i(I);
encode_s({literal,L}) -> encode_literal(L);
encode_s([]) -> encode_ival(?A, 0).

%%encode_s({lit,L}) -> encode_lit(L).

%% d = x|y
encode_d({x,X}) when ?is_reg(X) -> encode_ival(?X,X);
encode_d({y,Y})  when ?is_reg(Y) -> encode_ival(?Y,Y).

%% u = u (or raw unsigned integer)
encode_u({u,U}) when is_integer(U), U >= 0 -> encode_ival(?U, U);
encode_u(U)  when is_integer(U), U >= 0 -> encode_ival(?U, U).

 %% i = i (or raw integer)
encode_i({i,I}) when is_integer(I) -> encode_ival(?I, I);
encode_i(I) when is_integer(I) -> encode_ival(?I, I).

%% a = a|{atom,A}|atom
encode_a({atom,A}) when is_atom(A) -> encode_atom(A);
encode_a({a,I}) when is_integer(I), I >= 1 -> encode_ival(?A, I);
encode_a(A) when is_atom(A) ->    encode_atom(A).

encode_x({x,X}) when ?is_reg(X) -> encode_ival(?X,X).
encode_y({y,Y})  when ?is_reg(Y) -> encode_ival(?Y,Y).
encode_j({f,F}) -> encode_ival(?F,F).   %% j = jump target label
encode_l({fr,FR}) ->
    iolist_to_binary([encode_ival(?Z, ?FR),
		      encode_ival(?U, FR)]).

%% dl=d|l
encode_dl(Arg={fr,_}) -> encode_l(Arg);  %% floating point register
encode_dl(Arg) -> encode_d(Arg).

%% dlq=d|l|q
encode_dlq(Arg={fr,_}) -> encode_l(Arg);  %% floating point register
encode_dlq({literal,Arg}) -> encode_literal(Arg);  %% floating point register
encode_dlq(Arg) -> encode_d(Arg).

encode_st({string,Str0}, Stack=[Len|_]) ->
    <<Str:Len/binary,_/binary>> = Str0, %% trim if needed
    %% find Str among strings
    Ctx = jctx(),
    Strings = Ctx#jctx.strings,
    case binary:match(Str, Strings) of
	nomatch ->
	    Strings1 = <<Strings/binary, Str/binary>>,
	    jctx(Ctx#jctx{strings=Strings1}),
	    Offs = byte_size(Strings),
	    encode_ival(?U,Offs);
	{Offs,_Len} ->
	    encode_ival(?U,Offs)
    end.

%% fixme find bitstring....?
encode_stb({string,Str0}, [Bits|_]) ->
    Len = (Bits+7) div 8,
    <<Str:Len/binary,_/binary>> = Str0, %% trim if needed
    %% find Str among strings
    Ctx = jctx(),
    Strings = Ctx#jctx.strings,
    case binary:match(Str, Strings) of
	nomatch ->
	    Strings1 = <<Strings/binary, Str/binary>>,
	    jctx(Ctx#jctx{strings=Strings1}),
	    Offs = byte_size(Strings),
	    encode_ival(?U,Offs);
	{Offs,_Len} ->
	    encode_ival(?U,Offs)
    end.

encode_S({x,X}) when ?is_reg(X) ->   encode_ival(?X,X);
encode_S({y,Y})  when ?is_reg(Y) ->  encode_ival(?Y,Y).

encode_F(Arg) -> encode_import(Arg).

%% arity value
encode_A({u,A}) when A >= 0 ->   encode_ival(?U, A);
encode_A(A) when is_integer(A), A>=0 -> encode_ival(?U, A).

%% bit syntax flags encoded as u-tag
encode_G({u,U}) when is_integer(U) ->
    encode_ival(?U, U);
encode_G({field_flags,Flags}) when is_list(Flags) ->
    encode_ival(?U, encode_bit_flags(Flags));
encode_G(Flags) when is_list(Flags) ->
    encode_ival(?U, encode_bit_flags(Flags)).
    
encode_L([]) -> encode_ival(?U, 0);
encode_L(Line=[{location,_Fname,_Ln}]) -> encode_line(Line).

encode_R({list,L}) -> encode_R(L);
encode_R(L) when is_list(L) ->
    {N,Data} = encode_list(L),
    iolist_to_binary([encode_ival(?Z, ?LIST),
		      encode_ival(?U, N),
		      Data]).

encode_lit(Lit) ->
    Data = term_to_binary(Lit),
    N = byte_size(Data),
    iolist_to_binary([encode_ival(?Z, ?LITERAL),
		      encode_ival(?U, N),
		      Data]).

encode_bit_flags([little|Fs]) ->
    ?BIT_LITTLE bor encode_bit_flags(Fs);
encode_bit_flags([big|Fs]) ->
    ?BIT_BIG bor encode_bit_flags(Fs);
encode_bit_flags([native|Fs]) ->
    ?BIT_NATIVE bor encode_bit_flags(Fs);
encode_bit_flags([signed|Fs]) ->
    ?BIT_SIGNED bor encode_bit_flags(Fs);
encode_bit_flags([unsigned|Fs]) ->
    ?BIT_UNSIGNED bor encode_bit_flags(Fs);
encode_bit_flags([exact|Fs]) ->
    ?BIT_EXACT bor encode_bit_flags(Fs);
encode_bit_flags([]) ->
    0.

decode_bit_flags(U) ->
    if U band ?BIT_LITTLE =/= 0 -> [little]; true -> [big] end ++
    if U band ?BIT_SIGNED =/= 0 -> [signed]; true -> [unsigned] end ++
    if U band ?BIT_NATIVE =/= 0 -> [native]; true -> [] end ++
    if U band ?BIT_EXACT  =/= 0 -> [exact]; true -> [] end.

emit_(Arg) ->
    Enc = encode(Arg),
    ?verbose("arg ~p => ~p\n", [Arg, Enc]),
    emit_data(Enc).

emit_data(Code) ->
    case get('JCTX') of
	undefined -> error(missing_jit_begin);
	Ctx -> ram_file:write(Ctx#jctx.fd, Code)
    end.

encode(nil)     -> encode_ival(?A, 0);
encode(I) when is_integer(I) -> encode_ival(?U, I);
encode({atom,A}) when is_atom(A) -> encode_atom(A);
encode({integer,I}) when is_integer(I) -> encode_ival(?I,I);
encode(T) -> encode_(T).

encode_({u,U})   -> encode_ival(?U, U);
encode_({i,I})   -> encode_ival(?I, I);
encode_({a,I})   -> encode_ival(?A, I);
encode_({x,I})   -> encode_ival(?X, I);
encode_({y,I})   -> encode_ival(?Y, I);
encode_({f,I})   -> encode_ival(?F, I);
encode_({h,I})   -> encode_ival(?H, I);
encode_(FR={fr,_})  -> encode_l(FR);
encode_({atom,A}) when is_atom(A) -> encode_atom(A);
encode_({list,L}) ->
    {N,Data} = encode_list(L),
    iolist_to_binary([encode_ival(?Z, ?LIST),
		      encode_ival(?U, N),
		      Data]);
encode_({alloc,L}) ->
    encode_k(L);

encode_({literal,L}) -> %% inline literal
    U = insert_literal(L),    
    iolist_to_binary([encode_ival(?Z, ?LITERAL),
		      encode_ival(?U, U)]);
encode_({extfunc,M,F,A}) -> 
    encode_import(M,F,A);
encode_(Line=[{location,_Filename,_Line}]) ->
    encode_line(Line);
encode_([{location,Line}]) ->  %% FIXME
    encode_ival(?U, Line).

encode_ival(Tag,I) ->
    if I >= 0, I < 16 -> 
	    <<I:4,2#0:1,Tag:3>>;
       I >= 0, I < 2048 ->
	    <<(I bsr 8):3,2#01:2,Tag:3,I:8>>;
       true -> 
	    case i2b(I) of
		{N,Bin} when N =< 8 ->
		    <<(N-2):3,2#11:2,Tag:3, Bin/binary>>;
		{N,Bin} ->
		    Size = encode_ival(?U,N-9),
		    <<2#11111:5,Tag:3, Size/binary, Bin/binary>>
	    end
    end.

encode_list(List) when is_list(List) ->
    encode_list(List, 0, []).

encode_list([H|T], N, Acc) ->
    encode_list(T, N+1, [encode(H) | Acc]);
encode_list([], N, Acc) ->
    {N, lists:reverse(Acc)}.

encode_k({alloc,List}) ->
    {N,Data} = encode_alloc_list(List),
    iolist_to_binary([encode_ival(?Z, ?ALLOC),
		      encode_ival(?U, N),
		      Data]);
encode_k(U) when is_integer(U), U >= 0 -> 
    encode_u(U).


encode_alloc_list(List) when is_list(List) ->
    encode_alloc_list(List, 0, []).

encode_alloc_list([H|T], N, Acc) ->
    case H of
	{words, W} ->
	    encode_alloc_list(T,N+1,[[encode({u,?ALLOC_WORDS}),
				     encode({u,W})]|Acc]);
	{floats, F} ->
	    encode_alloc_list(T,N+1,[[encode({u,?ALLOC_FLOATS}),
				     encode({u,F})]|Acc]);
	{funs, F} ->
	    encode_alloc_list(T,N+1,[[encode({u,?ALLOC_FUNS}),
				      encode({u,F})]|Acc])
    end;
encode_alloc_list([], N, Acc) ->
    {N, lists:reverse(Acc)}.

decode(<<?LIST:4,0:1,?Z:3, Bin/binary>>) ->
    {{u,N},Bin1} = decode_ival(Bin),
    {List,Bin2} = decode_list(N, Bin1),
    {{list,List}, Bin2};
decode(<<?ALLOC:4,0:1,?Z:3, Bin/binary>>) ->
    {{u,N},Bin1} = decode_ival(Bin),
    {List,Bin2} = decode_alloc_list(N, Bin1),
    {{alloc,List}, Bin2};
decode(<<?LITERAL:4,0:1,?Z:3, Bin/binary>>) ->
    {{u,U},Bin1} = decode_ival(Bin),
    {decode_literal(U),Bin1};
    %% case decode_ival(Bin) of
    %% 	{{u,0},Bin1} -> {{lit,<<>>},Bin1};
    %% 	{{u,N},Bin1} ->
    %% 	    <<Data:N/binary,Bin2/binary>> = Bin1,
    %% 	    {{lit,binary_to_term(Data)},Bin2}
    %% end;
decode(<<?FR:4,0:1,?Z:3, Bin/binary>>) ->
    {{u,R},Bin1} = decode(Bin),
    {{fr,R}, Bin1};

decode(<<?TR:4,0:1,?Z:3, Bin/binary>>) ->
    {Reg,Bin1} = decode(Bin),
    {{u,TypeIndex},Bin2} = decode(Bin1),
    %% fixme load type info...
    {{tr,Reg,TypeIndex}, Bin2};

decode(Bin) ->
    case decode_ival(Bin) of
	{{a,A},Bin1} ->
	    {decode_atom(A), Bin1};
	Res ->
	     Res
    end.
    
%% decode primitive tags 0..6
decode_ival(<<2#11111:5,Tag:3,Bin/binary>>) -> 
    {{u,N},Bin1} = decode(Bin),
    <<Bin2:(N+9)/binary, Bin3/binary>> = Bin1,
    decode_tag(Tag, decode_ibytes(Bin2), Bin3);
decode_ival(<<N:3,2#11:2,Tag:3,Bin:(N+2)/binary,Bin1/binary>>) -> 
    decode_tag(Tag, decode_ibytes(Bin),Bin1);
decode_ival(<<IH:3,2#01:2,Tag:3,IL:8,Bin/binary>>) ->
    decode_tag(Tag, (IH bsl 8)+IL, Bin);
decode_ival(<<I:4,2#0:1,Tag:3,Bin/binary>>) -> 
    decode_tag(Tag, I, Bin).

decode_ibytes(Bin) ->
    <<Val:(byte_size(Bin)*8)/signed>> = Bin,
    Val.

decode_tag(Tag,Val,Bin) ->
    case Tag of
	?U -> {{u,Val},Bin};
	?I -> {{i,Val},Bin};
	?A -> {{a,Val},Bin};
	?X -> {{x,Val},Bin};
	?Y -> {{y,Val},Bin};
	?F -> {{f,Val},Bin};
	?H -> {{h,Val},Bin}
    end.

%% decode aRg list
decode_R(<<?LIST:4,0:1,?Z:3, Bin/binary>>) ->
    {{u,N},Bin1} = decode_ival(Bin),
    {List,Bin2} = decode_list(N, Bin1),
    {{list,List}, Bin2}.

decode_list(N, Bin) ->
   decode_list(N, Bin, []).

decode_list(0,Bin,Acc) ->
    {lists:reverse(Acc), Bin};
decode_list(I,Bin,Acc) ->
    {Arg,Bin1} = decode(Bin),
    decode_list(I-1,Bin1,[Arg|Acc]).

decode_arglist(Ts, Bin) ->
    decode_arglist(Ts, Bin, []).
    
decode_arglist([T|Ts], Bin, Acc) ->
    case T of
	j ->
	    {F={f,_},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[F|Acc]);
	'A' ->
	    {{u,U},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[U | Acc]);
	'L' ->
	    case decode(Bin) of
		{{u,0},Bin1} ->
		    decode_arglist(Ts,Bin1,[[] | Acc]);
		{{u,U},Bin1} ->
		    decode_arglist(Ts,Bin1,[decode_line(U) | Acc])
	    end;
	'E' ->
	    {{u,U},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[decode_export(U) | Acc]);
	'G' ->
	    {{u,U},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[decode_bit_flags(U) | Acc]);
	'U' -> 
	    {{u,U},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[U | Acc]);
	'F' ->
	    {{u,U},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[decode_import(U)|Acc]);
	'S' ->
	    case decode(Bin) of
		{X={x,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[X | Acc]);
		{Y={y,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Y | Acc])
	    end;
	'R' ->
	    {Args,Bin1} = decode_R(Bin),
	    decode_arglist(Ts,Bin1,[Args|Acc]);
	x -> 
	    {X={x,_},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[X | Acc]);
	y ->
	    {Y={y,_},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[Y | Acc]);
	a ->
	    {A={atom,_},Bin1} = decode(Bin), 
	    decode_arglist(Ts,Bin1,[A | Acc]);
	i ->
	    {{i,I},Bin1} = decode(Bin), 
	    decode_arglist(Ts,Bin1,[{integer,I} | Acc]);
	u -> 
	    {U={u,_},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[U | Acc]);
	n ->
	    {{a,0},Bin1} = decode(Bin), 
	    decode_arglist(Ts,Bin1,[[] | Acc]);
	k -> 
	    case decode(Bin) of 
		{A={alloc,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[A | Acc]);
%%		{X={x,_},Bin1} ->		
%%		    decode_arglist(Ts,Bin1,[X | Acc]);
		{{u,U},Bin1} ->
		    decode_arglist(Ts,Bin1,[U | Acc])
	    end;
	c ->
	    case decode(Bin) of
		{{i,I},Bin1} ->
		    decode_arglist(Ts,Bin1,[{integer,I} | Acc]);
		{{a,0},Bin1} ->
		    decode_arglist(Ts,Bin1,[[] | Acc]);
		{L={literal,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[L | Acc]);
		{{a,A},Bin1} ->
		    decode_arglist(Ts,Bin1,[decode_atom(A) | Acc])
	    end;
	s ->
	    case decode(Bin) of
		{{i,I},Bin1} ->
		    decode_arglist(Ts,Bin1,[{integer,I} | Acc]);
		{{a,0},Bin1} ->
		    decode_arglist(Ts,Bin1,[[] | Acc]);
		{L={literal,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[L | Acc]);
		{{u,U},Bin1} ->
		    decode_arglist(Ts,Bin1,[decode_literal(U)|Acc]);
		{{a,A},Bin1} ->
		    decode_arglist(Ts,Bin1,[decode_atom(A) | Acc]);
		{A={atom,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[A | Acc]);
		{X={x,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[X | Acc]);
		{Y={y,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Y | Acc]);
		{Tr={tr,_,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Tr | Acc])
	    end;
	d ->
	    case decode(Bin) of
		{X={x,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[X | Acc]);
		{Y={y,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Y | Acc])
	    end;
	dl ->
	    case decode(Bin) of
		{X={x,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[X | Acc]);
		{Y={y,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Y | Acc]);
		{FR={fr,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[FR | Acc])
	    end;
	dlq ->
	    case decode(Bin) of
		{X={x,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[X | Acc]);
		{Y={y,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Y | Acc]);
		{FR={fr,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[FR | Acc]);
		{_Q={u,U},Bin1} ->
		    decode_arglist(Ts,Bin1,[decode_literal(U)|Acc]);
		{Q={literal,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Q|Acc])
	    end;
	st ->
	    {{u,Offs},Bin1} = decode(Bin),
	    [Len|_] = Acc, %% assume length in previous argument
	    <<_:Offs/binary,Str:Len/binary,_/binary>> = (jctx())#jctx.strings,
	    decode_arglist(Ts,Bin1,[{string,Str}|Acc]);
	stb ->
	    {{u,Offs},Bin1} = decode(Bin),
	    [Bits|_] = Acc, %% assume bit-length in previous argument
	    Len = (Bits+7) div 8,
	    <<_:Offs/binary,Str:Len/binary,_/binary>> = (jctx())#jctx.strings,
	    decode_arglist(Ts,Bin1,[{string,Str}|Acc]);
	l ->
	    {FR={fr,_},Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[FR | Acc]);
	'_' ->
	    {Arg,Bin1} = decode(Bin),
	    decode_arglist(Ts,Bin1,[Arg|Acc])
    end;
decode_arglist([],Bin,Acc) ->
    {lists:reverse(Acc),Bin}.
	
decode_alloc_list(N, Bin) ->
   decode_alloc_list(N, Bin, []).

decode_alloc_list(0,Bin,Acc) ->
    {lists:reverse(Acc), Bin};
decode_alloc_list(I,Bin,Acc) ->
    case decode(Bin) of
	{{u,?ALLOC_WORDS},Bin1} ->
	    {{u,N},Bin2} = decode(Bin1),
	    decode_alloc_list(I-1,Bin2,[{words,N}|Acc]);
	{{u,?ALLOC_FLOATS},Bin1} ->
	    {{u,N},Bin2} = decode(Bin1),
	    decode_alloc_list(I-1,Bin2,[{floats,N}|Acc]);
	{{u,?ALLOC_FUNS},Bin1} ->
	    {{u,N},Bin2} = decode(Bin1),
	    decode_alloc_list(I-1,Bin2,[{funs,N}|Acc])
    end.

i2b(I) when I < 0 -> i2bn_(I,0,[]);
i2b(I) when I > 0 -> i2bp_(I,0,[]).

i2bp_(0,N,Acc) -> {N,list_to_binary(Acc)};
i2bp_(X,N,Acc) -> i2bp_(X bsr 8,N+1,[(X band 16#ff)|Acc]).

i2bn_(-1,N,Acc) -> {N,list_to_binary(Acc)};
i2bn_(X,N,Acc) -> i2bn_(X bsr 8,N+1,[(X band 16#ff)|Acc]).

load_binary(Binary) when is_binary(Binary) ->
    jinit_(),
    Res = load_(Binary),
    jterminate_(),
    Res.

load_file(Filename) when is_list(Filename) ->
    jinit_(),
    Res = load_(Filename),
    jterminate_(),
    Res.

load_(FilenameOrBinary) ->
    case beamjit_file:fold_chunks(FilenameOrBinary, fun load_chunk/3, []) of
	Error = {error,_} -> 
	    Error;
	Loaded ->
	    RChunks = resolve_chunks(Loaded, []),
	    {ok, RChunks}
    end.

load_chunk(<<"Atom">>, <<N:32,Atoms/binary>>, Acc) ->    
    Table = load_atoms(N, Atoms),
    ?verbose("Atom N=~w, table=~p\n", [N, Table]),
    [{atoms,Table}|Acc];

load_chunk(<<"AtU8">>, <<N:32,Data/binary>>, Acc) ->
    Table = load_atoms(N,Data),
    ?verbose("Atom N=~w, table=~p\n", [N, Table]),
    [{atoms,Table}|Acc];

load_chunk(<<"ExpT">>,<<N:32,Data/binary>>, Acc) ->
    Table = load_exports(N,Data),
    ?verbose("Export N=~w, table=~p\n", [N, Table]),
    [{exports,Table}|Acc];

load_chunk(<<"ImpT">>, <<N:32, Data/binary>>, Acc) ->
    Table = load_imports(N,Data),
    ?verbose("Import N=~w, table=~p\n", [N, Table]),
    [{imports,Table}|Acc];

load_chunk(<<"Code">>,<<SubSize:32,Chunk/binary>>, Acc) ->
    <<Info:SubSize/binary, Code/binary>> = Chunk,
    %% OpcodeSize = Size - SubSize - 8, %% 8 is size of CunkSize & SubSize
    %% <<OpCodes:OpcodeSize/binary, _Align/binary>> = Code,
    %% Asm = decode_opcodes(Code),
    CodeInfo = parse_code_info(Info),
    ?verbose("CodeInfo = ~p\n", [CodeInfo]),
    [{code,CodeInfo, Code}|Acc];

load_chunk(<<"StrT">>, <<Strings/binary>>, Acc) ->
    jctx((jctx())#jctx { strings = Strings }),
    [{strings,binary_to_list(Strings)} | Acc];

load_chunk(<<"Attr">>, TermBin, Acc) ->
    [{attributes,binary_to_term(TermBin)} | Acc];

load_chunk(<<"CInf">>, TermBin, Acc) ->
    CInfo = binary_to_term(TermBin),
    Source = proplists:get_value(source, CInfo),
    jctx((jctx())#jctx { source = Source }),
    [{compile_info,CInfo} | Acc];

load_chunk(<<"LocT">>, <<N:32, Locals/binary>>, Acc) ->
    [{locals,load_locals(N,Locals)} | Acc];

load_chunk(<<"LitT">>, <<_CompressedTableSize:32, Compressed/binary>>, Acc) ->
    <<N:32,Table/binary>> = zlib:uncompress(Compressed),
    Literals = load_literals(N,Table),
    [{literals,Literals} | Acc];

load_chunk(<<"Abst">>, <<>>, Acc) ->
    [{abstract_code,[]} | Acc];
load_chunk(<<"Abst">>, <<TermBin/binary>>, Acc) ->
    [{abstract_code,binary_to_term(TermBin)} | Acc];

load_chunk(<<"Line">>, <<Ver:32,Bits:32,NumLineInstrs:32,
			 NL:32,NF:32,Data/binary>>, Acc) ->
    Ls = load_lines(NL,NF,Data),
    [{line,[{version,Ver},{bits,Bits},
	    {num_line_instructions,NumLineInstrs},{lines,Ls}]} | Acc];

load_chunk(Name, Chunk, Acc) ->
    [{Name,Chunk} | Acc].

-define(LINES_VER, 0).
-define(LINES_BITS, 0).

%% FIXME? automatically add module info? optionally to be OTP compiliant?
build_module() ->
    Ctx = jctx(),
    %% write chunks:
    {NAtU8,AtU8} = build_atoms(),
    {ok,Code} = ram_file:get_file(Ctx#jctx.fd),
    NFunT = 0,
    FunT = <<>>,
    StrT = <<>>,
    {NImpT,ImpT} = build_imports(),    
    {NExpT,ExpT} = build_exports(),
    {NLitT,LitT} = build_literals(),
    {NLocT,LocT} = build_locals(),
    CInf = term_to_binary([{version,"1.0"},
			   {options, []},
			   {source, Ctx#jctx.source}]),
    %% - Dbgi?
    {NL,NF,NMax,Line} = build_lines(),
    NumLineInstrs = Ctx#jctx.num_lines,
    LineChunk = <<?LINES_VER:32,?LINES_BITS:32,
		  NumLineInstrs:32,
		  NL:32,NF:32,Line/binary>>,  %% NL-1
    %% open
    {ok,Fd} = beamjit_file:open_ram([write]),
    PatchMe = 0,
    beamjit_file:write_data(Fd, <<"FOR1",PatchMe:32,"BEAM">>),
    %% write chunks
    AtomChunk = <<NAtU8:32, AtU8/binary>>,
    beamjit_file:write_chunk(Fd, {<<"AtU8">>, AtomChunk}),
    %% write code chunk
    Instructionset = 0,
    OpcodeMax = Ctx#jctx.max_op,
    NumberOfLabels = Ctx#jctx.label+1,
    NumberOfFunctions = Ctx#jctx.func,
    CodeInfo = <<Instructionset:32/integer,
		 OpcodeMax:32/integer,
		 NumberOfLabels:32/integer,
		 NumberOfFunctions:32/integer>>,
    CodeChunk = <<(byte_size(CodeInfo)):32,
		  CodeInfo/binary, Code/binary>>,
    beamjit_file:write_chunk(Fd, {<<"Code">>, CodeChunk}),
    StringChunk = StrT,
    beamjit_file:write_chunk(Fd, {<<"StrT">>, StringChunk}),
    ImportChunk = <<NImpT:32,ImpT/binary>>,
    beamjit_file:write_chunk(Fd, {<<"ImpT">>, ImportChunk}),
    ExportChunk = <<NExpT:32,ExpT/binary>>,
    beamjit_file:write_chunk(Fd, {<<"ExpT">>, ExportChunk}),
    LambdaChunk = if NFunT =:= 0 ->
			  <<>>;
		     true ->
			  Chunk1 = <<NFunT:32,FunT/binary>>,
			  beamjit_file:write_chunk(Fd, {<<"FunT">>, Chunk1}),
			  Chunk1
		  end,
    LiteralChunk = if NLitT =:= 0 ->
			   <<>>;
		      true ->
			   Chunk2 = <<NLitT:32,LitT/binary>>,
			   beamjit_file:write_chunk(Fd, {<<"LitT">>, Chunk2}),
			   Chunk2
		   end,
    LocalChunk = <<NLocT:32,LocT/binary>>,
    beamjit_file:write_chunk(Fd, {<<"LocT">>,LocalChunk}),
    MD5 = erlang:md5([AtomChunk,CodeChunk,StringChunk,
		      ImportChunk,ExportChunk,LambdaChunk,LiteralChunk]),
    <<Vsn:128>> = MD5,
    Attr = term_to_binary([{vsn,[Vsn]}]),
    beamjit_file:write_chunk(Fd, {<<"Attr">>, Attr}),
    beamjit_file:write_chunk(Fd, {<<"CInf">>, CInf}),
    
    
    beamjit_file:write_chunk(Fd, {<<"Line">>, LineChunk}),
    %% patch size field
    RAM_Fd = beamjit_file:fd(Fd),
    {ok,Position} = file:position(RAM_Fd, eof),
    io:format("Position = ~w\n", [Position]),
    file:pwrite(RAM_Fd, 4, <<(Position-8):32>>),
    {ok,Mod} = ram_file:get_file(RAM_Fd),
    beamjit_file:close(Fd),
    Mod.

print_binary(Binary) when is_binary(Binary) ->
    print_(Binary).

print_file(Filename) when is_list(Filename) ->
    print_(Filename).

print_(FilenameOrBinary) ->
    jinit_(),
    Res = load_(FilenameOrBinary),
    jterminate_(),
    case Res of
	{ok,Chunks} ->
	    print_chunks(Chunks);
	Error ->
	    Error
    end.

print_chunks([{code,Opts,InstrList}|Chunks]) ->
    io:format("*** CODE ***\n"),
    print_functions(InstrList),
    io:format("*** CODEINFO ***\n"),
    lists:foreach(
      fun({K,V}) ->
	      io:format("  ~w: ~w\n", [K,V])
      end, Opts),
    print_chunks(Chunks);
print_chunks([_|Chunks]) ->
    print_chunks(Chunks);
print_chunks([]) ->
    ok.
    
%% "pretty" print code
print_functions([{func_info,{atom,Mod},{atom,Fun},Arity}|Cs]) ->
    io:format("FUNCTION ~s:~s/~w\n", [Mod,Fun,Arity]),
    print_functions(Cs);
print_functions([{label,L}|Cs]) ->
    io:format("~w:\n", [L]),
    print_functions(Cs);
print_functions([C|Cs]) ->
    io:format("    ~p\n", [C]),
    print_functions(Cs);
print_functions([]) ->
    ok.

%% Test that all instructions are encoded/decoded correctly
test_binary(Binary) when is_binary(Binary) ->
    test_(Binary).
test_file(Filename) when is_list(Filename) ->
    test_(Filename).

test_(FilenameOrBinary) ->
    jinit_(),
    case load_(FilenameOrBinary) of
	{ok, Chunks} ->
	    try test_chunks(Chunks) of
		_ -> ok
	    after
		jterminate_()
	    end;
	Error ->
	    jterminate_(),
	    Error
    end.

test_chunks([{code,_Info,InstructionList}|Chunks]) ->
    lists:foreach(
      fun(Instr) ->
	      IoList1 = [Opcode|_] = encode_instruction(Instr),
	      Data1 = iolist_to_binary(IoList1),
	      [Mnemonic|Args] = 
		  if is_atom(Instr) ->
			  [Instr];
		     is_tuple(Instr) ->
			  tuple_to_list(Instr)
		  end,
	      io:format("TEST mnemonic=~p, Args=~p\n", [Mnemonic, Args]),
	      file:position((jctx())#jctx.fd, 0),
	      file:truncate((jctx())#jctx.fd),
	      ok = apply(?MODULE,Mnemonic,Args),
	      {ok,Data2} = ram_file:get_file((jctx())#jctx.fd),
	      %% ?verbose("  Encoding1 = ~p\n", [Data1]),
	      %% ?verbose("  Encoding2 = ~p\n", [Data2]),
	      case {decode_instruction(Data1),
		    decode_instruction(Data2)} of
		  {{Opcode,Instr,<<>>},{Opcode,Instr,<<>>}} -> ok
	      end
      end, InstructionList),
    test_chunks(Chunks);
test_chunks([_|Chunks]) ->
    test_chunks(Chunks);
test_chunks([]) ->
    [].

%% pass2 resolve code and reverse all chunks 
resolve_chunks([{code,Info,Code}|Chunks], Acc) ->
    {Asm,NL,OpRangeSet} = decode_instruction_list(Code),
    set_num_lines(NL),
    All = gb_sets:from_list(lists:seq(1,?MAX_OP_CODE)),
    OpRangeNotUsed0 = gb_sets:difference(All,OpRangeSet),
    OpRangeNotUsed  = gb_sets:difference(OpRangeNotUsed0, obsolete()),
    OpCodeMaxUsed = gb_sets:largest(OpRangeSet),
    Info1 = [{opcode_range_num,gb_sets:size(OpRangeSet)},
	     {opcode_range_set,gb_sets_to_range_list(OpRangeSet)},
	     {opcode_range_not_used,gb_sets_to_range_list(OpRangeNotUsed)},
	     {opcode_max_used, OpCodeMaxUsed}
	    | Info],
    resolve_chunks(Chunks, [{code,Info1,Asm}|Acc]);
resolve_chunks([Chunk|Chunks], Acc) ->
    resolve_chunks(Chunks, [Chunk|Acc]);
resolve_chunks([], Acc) ->
    Acc.

obsolete() ->
    obsolete(?OTP_24).
obsolete(Version) ->
    maps:fold(
      fun(_Op,Opent,Set) ->
	      if Opent#opcode.vsn_deprecated =:= undefined ->
		      Set;
		 Opent#opcode.vsn_deprecated =< Version ->
		      gb_sets:add_element(Opent#opcode.op,Set);
		     true ->
		      Set
	      end
      end, gb_sets:new(), opcode_map()).

gb_sets_to_range_list(Set) ->
    range_list(gb_sets:to_list(Set)).

%% convert a sorted list of integers into ranges list
range_list([H|T]) ->
    range_list(H,H,T);
range_list([]) ->
    [].

range_list(L,H,[H1|T]) when H1 =:= H+1 ->
    range_list(L,H1,T);
range_list(L,L,[H1|T]) ->
    [L|range_list(H1,H1,T)];
range_list(L,H,[H1|T]) ->
    [{L,H}|range_list(H1,H1,T)];
range_list(L,L,[]) -> [L];
range_list(L,H,[]) -> [{L,H}].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Atom tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_atoms(N, Chunk) when is_integer(N), N>=0, is_binary(Chunk) ->
    Ctx = jctx(),
    {Items,Atoms} = load_items(fun load_atom/1,1,N,Chunk,Ctx#jctx.atoms),
    jctx(Ctx#jctx { atoms = Atoms }),
    Items.
    
load_atom(<<Len, Atom:Len/binary, Bin1/binary>>) ->
    {binary_to_atom(Atom), Bin1}.

build_atoms() ->
    Ctx = jctx(),
    N = num_items(Ctx#jctx.atoms)-1,
    {N, store_items(fun store_atom/1, 1, N, Ctx#jctx.atoms)}.

store_atom(A) when is_atom(A) ->
    Atom = atom_to_binary(A, utf8),
    Len  = byte_size(Atom),
    <<Len,Atom/binary>>.

decode_atom(I) when is_integer(I) ->
    case find_by_index(I, (jctx())#jctx.atoms) of
	error -> {a,I};
	{ok,A} -> {atom,A}
    end.

lookup_atom(A) when is_atom(A) ->
    get_index(A, (jctx())#jctx.atoms).

insert_atom(A) when is_atom(A) ->
    Ctx = jctx(),
    case find_by_item(A, Ctx#jctx.atoms) of
	error ->
	    {J, Atoms} = insert_item(Ctx#jctx.atoms, A),
	    jctx(Ctx#jctx { atoms = Atoms }),
	    J;
	{ok,I} ->
	    I
    end.

encode_atom(A) when is_atom(A) ->
    I = insert_atom(A),
    encode_ival(?A, I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Literal table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_literals(N, Chunk) when is_integer(N), N>=0, is_binary(Chunk) ->
    Ctx = jctx(),
    {Items,Literals} = load_items(fun load_literal/1,0,N,Chunk,
				  Ctx#jctx.literals),
    jctx(Ctx#jctx { literals = Literals }),
    Items.    
    
load_literal(<<Len:32,Literal:Len/binary,Bin1/binary>>) ->
    {binary_to_term(Literal), Bin1}.
    
build_literals() ->
    Ctx = jctx(),
    N = num_items(Ctx#jctx.literals),
    Table = store_items(fun store_literal/1, 0, N-1, Ctx#jctx.literals),
    UnCompressed = <<N:32, Table/binary>>,
    Compressed = zlib:compress(UnCompressed),
    {byte_size(Compressed), Compressed}.

store_literal(Term) ->
    Bin = term_to_binary(Term),
    Size = byte_size(Bin),
    <<Size:32,Bin/binary>>.

decode_literal(I) when is_integer(I) ->
    case find_by_index(I, (jctx())#jctx.literals) of
	error -> {u,I};
	{ok,L} -> {literal,L}
    end.

insert_literal(L) ->
    Ctx = jctx(),
    case find_by_item(L, Ctx#jctx.literals) of
	error ->
	    {J, Literals} = insert_item(Ctx#jctx.literals, L),
	    jctx(Ctx#jctx { literals = Literals }),
	    J;
	{ok,J} ->
	    J
    end.
    
encode_literal(L) ->
    I = insert_literal(L),
    encode_ival(?U,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  extern function mod:fun/arity tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_imports(N, Chunk) when is_integer(N), N>=0, is_binary(Chunk) ->
    Ctx = jctx(),
    {Items,Imports} = load_items(fun load_import/1,0,N,Chunk,
				 Ctx#jctx.imports),
    jctx(Ctx#jctx { imports = Imports }),
    Items.

load_import(<<M:32,F:32,Arity:32,Bin1/binary>>) ->
    {atom,Mod} = decode_atom(M),
    {atom,Fun} = decode_atom(F),
    {{Mod,Fun,Arity}, Bin1}.

build_imports() ->
    Ctx = jctx(),
    N = num_items(Ctx#jctx.imports),
    {N, store_items(fun store_import/1, 0, N-1, Ctx#jctx.imports)}.

store_import({Mod,Fun,A}) ->
    M = lookup_atom(Mod),
    F = lookup_atom(Fun),
    <<M:32,F:32,A:32>>.

decode_import(I) when is_integer(I) ->
    case find_by_index(I, (jctx())#jctx.imports) of
	error -> {u,I};
	{ok,{M,F,A}} -> {extfunc,M,F,A}
    end.

insert_import(Item) ->
    Ctx = jctx(),
    case find_by_item(Item, Ctx#jctx.imports) of
	error ->
	    {J, Imports} = insert_item(Ctx#jctx.imports, Item),
	    jctx(Ctx#jctx { imports = Imports }),
	    J;
	{ok,J} ->
	    J
    end.

encode_import({extfunc,M,F,A}) ->
    encode_import(M,F,A).
encode_import(M,F,A) ->
    _Mi = insert_atom(M),
    _Fi = insert_atom(F),
    Item = {M,F,A},
    I = insert_import(Item),
    encode_ival(?U,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  exported function fun/arity+entry tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_exports(N, Chunk) when is_integer(N), N>=0, is_binary(Chunk) ->
    Ctx = jctx(),
    {Items,Exports} = load_items(fun load_export/1,0,N,Chunk,
				 Ctx#jctx.exports),
    jctx(Ctx#jctx { exports = Exports }),
    Items.

load_export(<<F:32,Arity:32,Entry:32,Bin1/binary>>) ->
    {atom,Name} = decode_atom(F),
    {{Name,Arity,Entry}, Bin1}.

build_exports() ->
    Ctx = jctx(),
    N = num_items(Ctx#jctx.exports),
    {N, store_items(fun store_export/1, 0, N-1, Ctx#jctx.exports)}.

store_export({Name,Arity,Entry}) ->
    F = lookup_atom(Name),
    <<F:32,Arity:32,Entry:32>>.

decode_export(U) when is_integer(U), U >= 0 ->
    case find_by_index(U, (jctx())#jctx.exports) of
	error -> {u,U};
	{ok,{F,A,Entry}} -> {F,A,{f,Entry}}
    end.

insert_export(Name,Arity,Entry) ->
    Ctx = jctx(),
    Item = {Name,Arity,Entry},
    case find_by_item(Item, Ctx#jctx.exports) of
	error ->
	    {J, Exports} = insert_item(Ctx#jctx.exports, Item),
	    jctx(Ctx#jctx { exports = Exports }),
	    J;
	{ok,J} ->
	    J
    end.

encode_export(Name,Arity,{f,Entry}) ->
    I = insert_export(Name, Arity, Entry),
    encode_ival(?U,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  local function fun/arity entry tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_locals(N, Chunk) ->
    Ctx = jctx(),
    {Items,Locals} = load_items(fun load_local/1,0,N,Chunk,
				Ctx#jctx.locals),
    jctx(Ctx#jctx { locals = Locals }),
    Items.

load_local(<<F:32,Arity:32,Entry:32,Bin1/binary>>) ->
    {atom,Name} = decode_atom(F),
    {{Name, Arity, Entry},Bin1}.

build_locals() ->
    Ctx = jctx(),    
    N = num_items(Ctx#jctx.locals),
    {N, store_items(fun store_local/1, 0, N-1, Ctx#jctx.locals)}.

store_local({Name,Arity,Entry}) ->
    N = lookup_atom(Name),
    <<N:32,Arity:32,Entry:32>>.

decode_local(I) when is_integer(I) ->
    case find_by_index(I, (jctx())#jctx.locals) of
	error -> {u,I};
	{ok,{M,F,Entry}} -> {M,F,{f,Entry}}
    end.

insert_local(F,A,Entry) ->
    Ctx = jctx(),
    Item = {F,A,Entry},
    case find_by_item(Item, Ctx#jctx.locals) of
	error ->
	    {J, Locals} = insert_item(Ctx#jctx.locals, Item),
	    jctx(Ctx#jctx { locals = Locals }),
	    J;
	{ok,J} ->
	    J
    end.

encode_local(F,A,{f,Entry}) ->
    I = insert_local(F,A,Entry),
    encode_ival(?U,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  line tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_lines(NL, NF, Bin) ->
    ?verbose("load_lines NL=~w, NF=~w\n", [NL, NF]),
    load_lines_(Bin,NL,NF,1,0,[]).

load_lines_(Bin,0,NF,_I,_Fi,Acc0) ->
    Ctx = jctx(),
    Fns = list_to_tuple(load_linef(Bin,NF,[])),
    Lines = Ctx#jctx.lines,
    {Items,Map1} =
	lists:foldl(
	  fun({I,{0,Ln}},{Acc,Map}) ->
		  Fname0 = (jctx())#jctx.source,
		  Fname = filename:basename(Fname0),
		  Line = {Fname,Ln},
		  ?verbose("LOAD_LINE ~w = ~p\n", [I, Line]),
		  Map1 = Map#{ Line => I, I => Line },
		  {[{location,Fname,Ln}|Acc],Map1};
	     ({I,{Fi,Ln}},{Acc,Map}) ->
		  Fname = element(Fi, Fns),
		  Line = {Fname, Ln},
		  ?verbose("LOAD_LINE ~w = ~p\n", [I, Line]),
		  Map1 = Map#{ Line => I, I => Line },
		  {[{location,Fname,Ln}|Acc],Map1}
	  end, {[],Lines#jctx_table.map}, Acc0),
    Lines1 = Lines#jctx_table { map = Map1 },
    jctx(Ctx#jctx { lines = Lines1 }),
    Items;
load_lines_(Bin,N,NF,I,Fi,Acc) ->
    case decode_ival(Bin) of 
	{{i,L},Bin1} ->
	    Item = {I,{Fi,L}},
	    ?verbose("Load line item: ~p\n", [Item]),
	    load_lines_(Bin1,N-1,NF,I+1,Fi,[Item|Acc]);
	{{a,Fj},Bin1} -> %% NOTE! filename index not atom
	    ?verbose("Load file index: ~p\n", [Fj]),
	    load_lines_(Bin1,N,NF,I,Fj,Acc)
    end.

load_linef(_Bin, 0, Acc) ->
    lists:reverse(Acc);
load_linef(<<Size:16,F:Size/binary,Bin1/binary>>, I, Acc) ->
    load_linef(Bin1, I-1, [unicode:characters_to_list(F)|Acc]).

build_lines() ->
    Ctx = jctx(),
    Lines = Ctx#jctx.lines,
    Source = Ctx#jctx.source,
    LineList = maps:to_list(Lines#jctx_table.map),
    Fns0 = lists:usort([Fname || {_,{Fname,_Ln}} <- LineList]),
    Fns1 = Fns0 -- [Source],
    %% Map source-file to index
    FnMap = maps:from_list([{Source,0} |
			    lists:zip(Fns1,lists:seq(1, length(Fns1)))]),
    build_lines_(LineList, 0, FnMap, 0, []).

build_lines_([{_I,{Fname,Ln}}|List], J, FnMap, Max, Acc) ->
    case maps:get(Fname, FnMap) of
	J -> 
	    build_lines_(List, J, FnMap,
			 max(Max, Ln), [encode_ival(?I,Ln)|Acc]);
	J1 ->
	    build_lines_(List, J1, FnMap,
			 max(Max, Ln),
			 [encode_ival(?I,Ln),encode_ival(?A,J1)|Acc])
    end;
build_lines_([_|List], J, FnMap, Max, Acc) ->
    build_lines_(List, J, FnMap, Max, Acc);
build_lines_([], _J, FnMap, Max, Acc) ->
    Fn1 = tl(lists:sort([{Ix,Fn} || {Fn,Ix} <- maps:to_list(FnMap)])),
    NL = length(Acc),
    NF = length(Fn1),
    Line = 
	iolist_to_binary(
	  [lists:reverse(Acc),
	   [begin
		Bin = unicode:characters_to_binary(Fn, utf8),
		<<(byte_size(Bin)):16, Bin/binary>>
	    end || {_,Fn} <- Fn1]]),
    {NL,NF,Max,Line}.

decode_line(U) when is_integer(U) ->
    case find_by_index(U, (jctx())#jctx.lines) of    
	error -> {u,U};
	{ok,{F,Line}} -> [{location,F,Line}]
    end.

insert_line(Fname,Ln) ->
    Ctx = jctx(),
    Item = {Fname,Ln},
    case find_by_item(Item, Ctx#jctx.lines) of
	error ->
	    {J, Lines} = insert_item(Ctx#jctx.lines, Item),
	    jctx(Ctx#jctx { lines = Lines }),
	    J;
	{ok,J} ->
	    J
    end.

encode_line([]) ->
    encode_ival(?U,0);
encode_line([{location,Fname,Ln}]) ->
    I = insert_line(Fname, Ln),
    encode_ival(?U,I).

%%  decode chunk items
load_items(Fun, First, N, Chunk, Table) ->
    {Items,Map} = load_items_(Fun, First, N, Chunk, [], Table#jctx_table.map),
    {Items,Table#jctx_table { index = N, map = Map }}.

load_items_(_Fun,_I,_N,<<>>,Acc,Map) ->
    {lists:reverse(Acc), Map};
load_items_(Fun,I,N,Bin,Acc,Map) ->
    {Item,Bin1} = Fun(Bin),
    Map1 = Map#{ I => Item, Item => I },
    load_items_(Fun,I+1,N,Bin1,[Item|Acc],Map1).

%%  encode chunk items
store_items(Fun, First, Max, #jctx_table{map=Map}) ->
    store_items_(Fun, First, Max, Map, []).

store_items_(Fun, I, Max, Map, Acc) when I =< Max ->
    Data = maps:get(I, Map),
    Bin = Fun(Data),
    store_items_(Fun, I+1, Max, Map, [Bin|Acc]);
store_items_(_Fun, _I, _Max, _Map, Acc) ->
    iolist_to_binary(lists:reverse(Acc)).

find_by_item(Item, #jctx_table{map=Map}) ->
    maps:find(Item, Map).

find_by_index(Index, #jctx_table{map=Map}) when is_integer(Index) ->
    maps:find(Index, Map).

num_items(#jctx_table{index=N}) ->
    N.

get_index(Item, #jctx_table{map=Map}) ->
    maps:get(Item, Map).

insert_item(Table=#jctx_table{map=Map,index=I}, Item) ->
    Map1 = Map#{ Item => I, I => Item },
    J = I+1,
    {I, Table#jctx_table{map=Map1,index=J}}.

%% Other chunks

parse_code_info(<<Instructionset:32/integer,
		  OpcodeMax:32/integer,
		  NumberOfLabels:32/integer,
		  NumberOfFunctions:32/integer,
		  Rest/binary>>) ->
    [ {instructionset, Instructionset},
      {opcode_max, OpcodeMax},
      {number_of_labels, NumberOfLabels},
      {number_of_functions, NumberOfFunctions} |
      case Rest of
	  <<>> -> [];
	  _ -> [{newinfo, Rest}]
      end].

decode_instruction_list(Bin) when is_binary(Bin) ->
    decode_instruction_list(Bin, gb_sets:new(), 0, []).

decode_instruction_list(<<>>, OpSet, NLn, Acc) ->
    {lists:reverse(Acc), NLn, OpSet};
decode_instruction_list(Bin, OpSet, NLn, Acc) ->
    {Opcode,Instr,Bin1} = decode_instruction(Bin),
    ?verbose("opcode ~p, instruction=~p\n", [Opcode,Instr]),
    case Instr of
	{line,_} ->
	    decode_instruction_list(Bin1, gb_sets:add_element(Opcode, OpSet),
				    NLn+1, [Instr|Acc]);
	int_code_end ->
	    OpSet1 = gb_sets:add_element(Opcode, OpSet),
	    {lists:reverse(Acc), NLn, OpSet1};
	_ ->
	    decode_instruction_list(Bin1, gb_sets:add_element(Opcode, OpSet),
				    NLn, [Instr|Acc])
    end.

decode_instruction(<<Opcode,Bin/binary>>) ->
    #opcode{mnemonic=Mnemonic,argtypes=ArgTypes} = 
	maps:get(Opcode, opcode_map()),
    case decode_arglist(ArgTypes,Bin) of
	{[],Bin1} ->
	    {Opcode,Mnemonic,Bin1};
	{Args,Bin1} ->
	    {Opcode,list_to_tuple([Mnemonic|Args]),Bin1}
    end.


inc_num_lines() ->
    Ctx = jctx(),
    set_num_lines(Ctx,Ctx#jctx.num_lines + 1).

set_num_lines(NL) ->
    set_num_lines(jctx(),NL).
set_num_lines(Ctx,NL) ->
    jctx(Ctx#jctx{num_lines=NL}),
    ok.


-spec opcode_map() -> #{ integer() => #opcode{},
			 atom() => op() }.

-define(OPENT(Op,Mnemonic,As),
	(Op) => #opcode{mnemonic=(Mnemonic),
			op=(Op),
			arity = length(As),
			vsn_introduced=?DEFAULT_VSN,
			argtypes = (As)
		       },
	(Mnemonic) => (Op)).

-define(OPENT(Op,Mnemonic,As,Vsn0),
	(Op) => #opcode{mnemonic=(Mnemonic),
			op=(Op),
			arity=length(As),
			vsn_introduced=(Vsn0),
			argtypes = (As)
		       },
	(Mnemonic) => (Op)).

-define(OPENT(Op,Mnemonic,As,Vsn0,Vsn1),
	(Op) => #opcode{mnemonic=(Mnemonic),
			op=(Op),
			arity=length(As),
			vsn_introduced=(Vsn0),
			vsn_deprecated=(Vsn1),
			argtypes=(As)
		       },
	(Mnemonic) => (Op)).


-define(LABEL, 1).
-define(FUNC_INFO, 2).
-define(INT_CODE_END, 3).
-define(CALL, 4).
-define(CALL_LAST, 5).
-define(CALL_ONLY, 6).
-define(CALL_EXT, 7).
-define(CALL_EXT_LAST, 8).
-define(BIF0, 9).
-define(BIF1, 10).
-define(BIF2, 11).
-define(ALLOCATE, 12).
-define(ALLOCATE_HEAP, 13).
-define(ALLOCATE_ZERO, 14).
-define(ALLOCATE_HEAP_ZERO, 15).
-define(TEST_HEAP, 16).
-define(INIT, 17).
-define(DEALLOCATE, 18).
-define(RETURN, 19).
-define(SEND, 20).
-define(REMOVE_MESSAGE, 21).
-define(TIMEOUT, 22).
-define(LOOP_REC, 23).
-define(LOOP_REC_END, 24).
-define(WAIT, 25).
-define(WAIT_TIMEOUT, 26).
-define(M_PLUS, 27).
-define(M_MINUS, 28).
-define(M_TIMES, 29).
-define(M_DIV, 30).
-define(INT_DIV, 31).
-define(INT_REM, 32).
-define(INT_BAND, 33).
-define(INT_BOR, 34).
-define(INT_BXOR, 35).
-define(INT_BSL, 36).
-define(INT_BSR, 37).
-define(INT_BNOT, 38).
-define(IS_LT, 39).
-define(IS_GE, 40).
-define(IS_EQ, 41).
-define(IS_NE, 42).
-define(IS_EQ_EXACT, 43).
-define(IS_NE_EXACT, 44).
-define(IS_INTEGER, 45).
-define(IS_FLOAT, 46).
-define(IS_NUMBER, 47).
-define(IS_ATOM, 48).
-define(IS_PID, 49).
-define(IS_REFERENCE, 50).
-define(IS_PORT, 51).
-define(IS_NIL, 52).
-define(IS_BINARY, 53).
-define(IS_CONSTANT, 54).
-define(IS_LIST, 55).
-define(IS_NONEMPTY_LIST, 56).
-define(IS_TUPLE, 57).
-define(TEST_ARITY, 58).
-define(SELECT_VAL, 59).
-define(SELECT_TUPLE_ARITY, 60).
-define(JUMP, 61).
-define(CATCH, 62).
-define(CATCH_END, 63).
-define(MOVE, 64).
-define(GET_LIST, 65).
-define(GET_TUPLE_ELEMENT, 66).
-define(SET_TUPLE_ELEMENT, 67).
-define(PUT_STRING, 68).
-define(PUT_LIST, 69).
-define(PUT_TUPLE, 70).
-define(PUT, 71).
-define(BADMATCH, 72).
-define(IF_END, 73).
-define(CASE_END, 74).
-define(CALL_FUN, 75).
-define(MAKE_FUN, 76).
-define(IS_FUNCTION, 77).
-define(CALL_EXT_ONLY, 78).
-define(BS_START_MATCH, 79).
-define(BS_GET_INTEGER, 80).
-define(BS_GET_FLOAT, 81).
-define(BS_GET_BINARY, 82).
-define(BS_SKIP_BITS, 83).
-define(BS_TEST_TAIL, 84).
-define(BS_SAVE, 85).
-define(BS_RESTORE, 86).
-define(BS_INIT, 87).
-define(BS_FINAL, 88).
-define(BS_PUT_INTEGER, 89).
-define(BS_PUT_BINARY, 90).
-define(BS_PUT_FLOAT, 91).
-define(BS_PUT_STRING, 92).
-define(BS_NEED_BUF, 93).
-define(FCLEARERROR, 94).
-define(FCHECKERROR, 95).
-define(FMOVE, 96).
-define(FCONV, 97).
-define(FADD, 98).
-define(FSUB, 99).
-define(FMUL, 100).
-define(FDIV, 101).
-define(FNEGATE, 102).
-define(MAKE_FUN2, 103).
-define(TRY, 104).
-define(TRY_END, 105).
-define(TRY_CASE, 106).
-define(TRY_CASE_END, 107).
-define(RAISE, 108).
-define(BS_INIT2, 109).
-define(BS_BITS_TO_BYTES, 110).
-define(BS_ADD, 111).
-define(APPLY, 112).
-define(APPLY_LAST, 113).
-define(IS_BOOLEAN, 114).
-define(IS_FUNCTION2, 115).
-define(BS_START_MATCH2, 116).
-define(BS_GET_INTEGER2, 117).
-define(BS_GET_FLOAT2, 118).
-define(BS_GET_BINARY2, 119).
-define(BS_SKIP_BITS2, 120).
-define(BS_TEST_TAIL2, 121).
-define(BS_SAVE2, 122).
-define(BS_RESTORE2, 123).
-define(GC_BIF1, 124).
-define(GC_BIF2, 125).
-define(BS_FINAL2, 126).
-define(BS_BITS_TO_BYTES2, 127).
-define(PUT_LITERAL, 128).
-define(IS_BITSTR, 129).
-define(BS_CONTEXT_TO_BINARY, 130).
-define(BS_TEST_UNIT, 131).
-define(BS_MATCH_STRING, 132).
-define(BS_INIT_WRITABLE, 133).
-define(BS_APPEND, 134).
-define(BS_PRIVATE_APPEND, 135).
-define(TRIM, 136).
-define(BS_INIT_BITS, 137).
-define(BS_GET_UTF8, 138).
-define(BS_SKIP_UTF8, 139).
-define(BS_GET_UTF16, 140).
-define(BS_SKIP_UTF16, 141).
-define(BS_GET_UTF32, 142).
-define(BS_SKIP_UTF32, 143).
-define(BS_UTF8_SIZE, 144).
-define(BS_PUT_UTF8, 145).
-define(BS_UTF16_SIZE, 146).
-define(BS_PUT_UTF16, 147).
-define(BS_PUT_UTF32, 148).
-define(ON_LOAD, 149).
-define(RECV_MARK, 150).
-define(RECV_SET, 151).
-define(GC_BIF3, 152).
-define(LLINE, 153).
-define(PUT_MAP_ASSOC, 154).
-define(PUT_MAP_EXACT, 155).
-define(IS_MAP, 156).
-define(HAS_MAP_FIELDS, 157).
-define(GET_MAP_ELEMENTS, 158).
-define(IS_TAGGED_TUPLE, 159).
-define(BUILD_STACKTRACE, 160).
-define(RAW_RAISE, 161).
-define(GET_HD, 162).
-define(GET_TL, 163).
-define(PUT_TUPLE2, 164).
-define(BS_GET_TAIL, 165).
-define(BS_START_MATCH3, 166).
-define(BS_GET_POSITION, 167).
-define(BS_SET_POSITION, 168).
-define(SWAP, 169).
-define(BS_START_MATCH4, 170).
-define(MAKE_FUN3, 171).
-define(INIT_YREGS, 172).
-define(RECV_MARKER_BIND, 173).
-define(RECV_MARKER_CLEAR, 174).
-define(RECV_MARKER_RESERVE, 175).
-define(RECV_MARKER_USE, 176).
-define(BS_CREATE_BIN, 177).
-define(CALL_FUN2, 178).
-define(NIF_START, 179).
-define(BADRECORD, 180).

opcode_map() ->
#{
?OPENT(?LABEL,label,['U']),
?OPENT(?FUNC_INFO,func_info,[a,a,'U']),
?OPENT(?INT_CODE_END,int_code_end,[]),
?OPENT(?CALL,call,['A',j]),
?OPENT(?CALL_LAST,call_last,['A',j,'U']),
?OPENT(?CALL_ONLY,call_only,['A',j]),
?OPENT(?CALL_EXT,call_ext,['A','F']),
?OPENT(?CALL_EXT_LAST,call_ext_last,['A','F','U']),
?OPENT(?BIF0,bif0,['F',d]),
?OPENT(?BIF1,bif1,[j,'F',s,d]),
?OPENT(?BIF2,bif2,[j,'F',s,s,d]),
?OPENT(?ALLOCATE,allocate,['U','U']),
?OPENT(?ALLOCATE_HEAP,allocate_heap,['U',k,'U']),
?OPENT(?ALLOCATE_ZERO,allocate_zero,['U','U'], ?DEFAULT_VSN, ?OTP_24),
?OPENT(?ALLOCATE_HEAP_ZERO,allocate_heap_zero,['U','U','U'], ?DEFAULT_VSN, ?OTP_24),
?OPENT(?TEST_HEAP,test_heap,[k,'U']),
?OPENT(?INIT,init,[d], ?DEFAULT_VSN, ?OTP_24),
?OPENT(?DEALLOCATE,deallocate,['U']),
?OPENT(?RETURN,return,[]),
?OPENT(?SEND,send,[]),
?OPENT(?REMOVE_MESSAGE,remove_message,[]),
?OPENT(?TIMEOUT,timeout,[]),
?OPENT(?LOOP_REC,loop_rec,[j,d]),
?OPENT(?LOOP_REC_END,loop_rec_end,[j]),
?OPENT(?WAIT,wait,[j]),
?OPENT(?WAIT_TIMEOUT,wait_timeout,[j,s]),
?OPENT(?M_PLUS,m_plus,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?M_MINUS,m_minus,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?M_TIMES,m_times,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?M_DIV,m_div,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?INT_DIV,int_div,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?INT_REM,int_rem,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?INT_BAND,int_band,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?INT_BOR,int_bor,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?INT_BXOR,int_bxor,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?INT_BSL,int_bsl,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?INT_BSR,int_bsr,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?INT_BNOT,int_bnot,[j,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?IS_LT,is_lt,[j,s,s]),
?OPENT(?IS_GE,is_ge,[j,s,s]),
?OPENT(?IS_EQ,is_eq,[j,s,s]),
?OPENT(?IS_NE,is_ne,[j,s,s]),
?OPENT(?IS_EQ_EXACT,is_eq_exact,[j,s,s]),
?OPENT(?IS_NE_EXACT,is_ne_exact,[j,s,s]),
?OPENT(?IS_INTEGER,is_integer,[j,s]),
?OPENT(?IS_FLOAT,is_float,[j,s]),
?OPENT(?IS_NUMBER,is_number,[j,s]),
?OPENT(?IS_ATOM,is_atom,[j,s]),
?OPENT(?IS_PID,is_pid,[j,s]),
?OPENT(?IS_REFERENCE,is_reference,[j,s]),
?OPENT(?IS_PORT,is_port,[j,s]),
?OPENT(?IS_NIL,is_nil,[j,s]),
?OPENT(?IS_BINARY,is_binary,[j,s]),
?OPENT(?IS_CONSTANT,is_constant,[j,s], ?OTP_R4, ?OTP_R13B_03),
?OPENT(?IS_LIST,is_list,[j,s]),
?OPENT(?IS_NONEMPTY_LIST,is_nonempty_list,[j,s]),
?OPENT(?IS_TUPLE,is_tuple,[j,s]),
?OPENT(?TEST_ARITY,test_arity,[j,s,'A']),
?OPENT(?SELECT_VAL,select_val,[s,j,'R']),
?OPENT(?SELECT_TUPLE_ARITY,select_tuple_arity,[s,j,'R']),
?OPENT(?JUMP,jump,[j]),
?OPENT(?CATCH,'catch',[d,j]),
?OPENT(?CATCH_END,catch_end,[d]),
?OPENT(?MOVE,move,[s,d]),
?OPENT(?GET_LIST,get_list,[s,d,d]),
?OPENT(?GET_TUPLE_ELEMENT,get_tuple_element,[s,'U',d]),
?OPENT(?SET_TUPLE_ELEMENT,set_tuple_element,[s,d,'U']),
?OPENT(?PUT_STRING,put_string,['_','_','_'],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?PUT_LIST,put_list,[s,s,d]),
?OPENT(?PUT_TUPLE,put_tuple,['U',d],?DEFAULT_VSN,?OTP_22),
?OPENT(?PUT,put,[s],?DEFAULT_VSN,?OTP_22),
?OPENT(?BADMATCH,badmatch,['_']),
?OPENT(?IF_END,if_end,[]),
?OPENT(?CASE_END,case_end,['_']),
?OPENT(?CALL_FUN,call_fun,['U']),
?OPENT(?MAKE_FUN,make_fun,['_','_','_'],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(?IS_FUNCTION,is_function,['_','_']),
?OPENT(?CALL_EXT_ONLY,call_ext_only,['A','F'],?OTP_R5, ?OTP_R13B_03),
?OPENT(?BS_START_MATCH,bs_start_match,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(?BS_GET_INTEGER,bs_get_integer,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(?BS_GET_FLOAT,bs_get_float,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(?BS_GET_BINARY,bs_get_binary,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(?BS_SKIP_BITS,bs_skip_bits,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(?BS_TEST_TAIL,bs_test_tail,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(?BS_SAVE,bs_save,['_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(?BS_RESTORE,bs_restore,['_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(?BS_INIT,bs_init,['_','_'],?OTP_R7A, ?OTP_R13B_03),
?OPENT(?BS_FINAL,bs_final,['_','_'],?OTP_R7A, ?OTP_R13B_03),
?OPENT(?BS_PUT_INTEGER,bs_put_integer,[j,'_','_','G','_'],?OTP_R7A),
?OPENT(?BS_PUT_BINARY,bs_put_binary,[j,'_','_','G','_'],?OTP_R7A),
?OPENT(?BS_PUT_FLOAT,bs_put_float,[j,'_','_','G','_'],?OTP_R7A),
?OPENT(?BS_PUT_STRING,bs_put_string,['U',st],?OTP_R7A),
?OPENT(?BS_NEED_BUF,bs_need_buf,['_'],?OTP_R7B, ?OTP_R13B_03),
?OPENT(?FCLEARERROR,fclearerror,[],?OTP_R8,?OTP_24),
?OPENT(?FCHECKERROR,fcheckerror,['_'],?OTP_R8,?OTP_24),
?OPENT(?FMOVE,fmove,[dlq,dl],?OTP_R8),
?OPENT(?FCONV,fconv,['S',l],?OTP_R8),
?OPENT(?FADD,fadd,[j,l,l,l],?OTP_R8),
?OPENT(?FSUB,fsub,[j,l,l,l],?OTP_R8),
?OPENT(?FMUL,fmul,[j,l,l,l],?OTP_R8),
?OPENT(?FDIV,fdiv,[j,l,l,l],?OTP_R8),
?OPENT(?FNEGATE,fnegate,[j,l,l],?OTP_R8),
?OPENT(?MAKE_FUN2,make_fun2,['_'],?OTP_R8,?OTP_24),
?OPENT(?TRY,'try',['S',j],?OTP_R10B),
?OPENT(?TRY_END,try_end,['S'],?OTP_R10B),
?OPENT(?TRY_CASE,try_case,['S'],?OTP_R10B),
?OPENT(?TRY_CASE_END,try_case_end,[s],?OTP_R10B),
?OPENT(?RAISE,raise,['S','S'],?OTP_R10B),
?OPENT(?BS_INIT2,bs_init2,[j,s,'U','U','G',s],?OTP_R10B),
?OPENT(?BS_BITS_TO_BYTES,bs_bits_to_bytes,['_','_','_'],?OTP_R10B, ?OTP_R13B_03),
?OPENT(?BS_ADD,bs_add,['_','_','_','_','_'],?OTP_R10B),
?OPENT(?APPLY,apply,['_'],?OTP_R10B),
?OPENT(?APPLY_LAST,apply_last,['_','_'],?OTP_R10B),
?OPENT(?IS_BOOLEAN,is_boolean,['_','_'],?OTP_R10B),
?OPENT(?IS_FUNCTION2,is_function2,['_','_','_'],?OTP_R10B_6),
?OPENT(?BS_START_MATCH2,bs_start_match2,[j,'_','_','_','_'],?OTP_R11B,?OTP_22),
?OPENT(?BS_GET_INTEGER2,bs_get_integer2,[j,'_','_','_','_','G','_'],?OTP_R11B,?OTP_22),
?OPENT(?BS_GET_FLOAT2,bs_get_float2,[j,'_','_','_','_','G','_'],?OTP_R11B,?OTP_22),
?OPENT(?BS_GET_BINARY2,bs_get_binary2,[j,'_','_','_','_','G','_'],?OTP_R11B,?OTP_22),
?OPENT(?BS_SKIP_BITS2,bs_skip_bits2,[j,'_','_','_','G'],?OTP_R11B,?OTP_22),
?OPENT(?BS_TEST_TAIL2,bs_test_tail2,[j,'_','_'],?OTP_R11B,?OTP_22),
?OPENT(?BS_SAVE2,bs_save2,['_','_'],?OTP_R11B,?OTP_22),
?OPENT(?BS_RESTORE2,bs_restore2,['_','_'],?OTP_R11B,?OTP_22),
?OPENT(?GC_BIF1,gc_bif1,[j,'U','F','_','_'],?OTP_R11B),
?OPENT(?GC_BIF2,gc_bif2,[j,'U','F','_','_','_'],?OTP_R11B),
?OPENT(?BS_FINAL2,bs_final2,['_','_'],?OTP_R11B,?OTP_R12B),
?OPENT(?BS_BITS_TO_BYTES2,bs_bits_to_bytes2,['_','_'],?OTP_R11B,?OTP_R12B),
?OPENT(?PUT_LITERAL,put_literal,['_','_'],?OTP_R11B_4,?OTP_R12B),
?OPENT(?IS_BITSTR,is_bitstr,['_','_'],?OTP_R11B_5),
?OPENT(?BS_CONTEXT_TO_BINARY,bs_context_to_binary,['_'],?OTP_R12B,?OTP_22),
?OPENT(?BS_TEST_UNIT,bs_test_unit,[j,'_','_'],?OTP_R12B),
?OPENT(?BS_MATCH_STRING,bs_match_string,[j,'_','U',stb],?OTP_R12B),
?OPENT(?BS_INIT_WRITABLE,bs_init_writable,[],?OTP_R12B),
?OPENT(?BS_APPEND,bs_append,[j,'_','_','_','_','_','G','_'],?OTP_R12B),
?OPENT(?BS_PRIVATE_APPEND,bs_private_append,[j,'_','_','_','G','_'],?OTP_R12B),
?OPENT(?TRIM,trim,['U','U'],?OTP_R12B),
?OPENT(?BS_INIT_BITS,bs_init_bits,[j,'_','_','_','G','_'],?OTP_R12B),
?OPENT(?BS_GET_UTF8,bs_get_utf8,[j,'_','_','G','_'],?OTP_R12B_5),
?OPENT(?BS_SKIP_UTF8,bs_skip_utf8,[j,'_','_','G'],?OTP_R12B_5),
?OPENT(?BS_GET_UTF16,bs_get_utf16,[j,'_','_','G','_'],?OTP_R12B_5),
?OPENT(?BS_SKIP_UTF16,bs_skip_utf16,[j,'_','_','G'],?OTP_R12B_5),
?OPENT(?BS_GET_UTF32,bs_get_utf32,[j,'_','_','G','_'],?OTP_R12B_5),
?OPENT(?BS_SKIP_UTF32,bs_skip_utf32,[j,'_','_','G'],?OTP_R12B_5),
?OPENT(?BS_UTF8_SIZE,bs_utf8_size,[j,'_','_'],?OTP_R12B_5),
?OPENT(?BS_PUT_UTF8,bs_put_utf8,[j,'G',s],?OTP_R12B_5),
?OPENT(?BS_UTF16_SIZE,bs_utf16_size,[j,'_','_'],?OTP_R12B_5),
?OPENT(?BS_PUT_UTF16,bs_put_utf16,[j,'G',s],?OTP_R12B_5),
?OPENT(?BS_PUT_UTF32,bs_put_utf32,[j,'G',s],?OTP_R12B_5),
?OPENT(?ON_LOAD,on_load,[],?OTP_R13B_03),
?OPENT(?RECV_MARK,recv_mark,['_'],?OTP_R14A,?OTP_24),
?OPENT(?RECV_SET,recv_set,['_'],?OTP_R14A,?OTP_24),
?OPENT(?GC_BIF3,gc_bif3,[j,'U','F','_','_','_','_'],?OTP_R14A),
?OPENT(?LLINE,line,['L'],?OTP_R15A),
?OPENT(?PUT_MAP_ASSOC,put_map_assoc,[j,s,d,'U','R'],?OTP_R17),
?OPENT(?PUT_MAP_EXACT,put_map_exact,[j,s,d,'U','R'],?OTP_R17),
?OPENT(?IS_MAP,is_map,[j,s],?OTP_R17),
?OPENT(?HAS_MAP_FIELDS,has_map_fields,[j,s,'R'],?OTP_R17),
?OPENT(?GET_MAP_ELEMENTS,get_map_elements,[j,s,'R'],?OTP_R17),
?OPENT(?IS_TAGGED_TUPLE,is_tagged_tuple,[j,s,'U',a],?OTP_20),
?OPENT(?BUILD_STACKTRACE,build_stacktrace,[],?OTP_21),
?OPENT(?RAW_RAISE,raw_raise,[],?OTP_21),
?OPENT(?GET_HD,get_hd,[s,d],?OTP_21),
?OPENT(?GET_TL,get_tl,[s,d],?OTP_21),
?OPENT(?PUT_TUPLE2,put_tuple2,[d,'R'],?OTP_22),
?OPENT(?BS_GET_TAIL,bs_get_tail,[s,d,'U'],?OTP_22),
?OPENT(?BS_START_MATCH3,bs_start_match3,[j,'_','_','_'],?OTP_22),
?OPENT(?BS_GET_POSITION,bs_get_position,['_','_','_'],?OTP_22),
?OPENT(?BS_SET_POSITION,bs_set_position,['_','_'],?OTP_22),
?OPENT(?SWAP,swap,['_','_'],?OTP_23),
?OPENT(?BS_START_MATCH4,bs_start_match4,['_','_','_','_'],?OTP_23),
?OPENT(?MAKE_FUN3,make_fun3,['_','_','_'],?OTP_24),
?OPENT(?INIT_YREGS,init_yregs,['_'],?OTP_24),
?OPENT(?RECV_MARKER_BIND,recv_marker_bind,['_','_'],?OTP_24),
?OPENT(?RECV_MARKER_CLEAR,recv_marker_clear,['_'],?OTP_24),
?OPENT(?RECV_MARKER_RESERVE,recv_marker_reserve,['_'],?OTP_24),
?OPENT(?RECV_MARKER_USE,recv_marker_use,['_'],?OTP_24),
?OPENT(?BS_CREATE_BIN,bs_create_bin,[j,'_','_','_','_','_'],?OTP_25),
?OPENT(?CALL_FUN2,call_fun2,['_','A',s],?OTP_25),
?OPENT(?NIF_START,nif_start,[],?OTP_25),
?OPENT(?BADRECORD,badrecord,['_'],?OTP_25)
}.

is_bif(Name, Arity)  when is_atom(Name), is_integer(Arity), Arity >= 0 ->
    %% FIXME: we may call runtime like this for JIT but not for compiling
    %% and saving
    erlang:is_builtin(erlang, Name, Arity).
    
-spec is_gc_bif(atom(), Arity::integer()) -> boolean().
is_gc_bif(hd, 1) -> false;
is_gc_bif(tl, 1) -> false;
is_gc_bif(self, 0) -> false;
is_gc_bif(node, 0) -> false;
is_gc_bif(node, 1) -> false;
is_gc_bif(element, 2) -> false;
is_gc_bif(get, [_]) -> 1;
is_gc_bif(is_map_key, 2) -> false;
is_gc_bif(map_get, 2) -> false;
is_gc_bif(tuple_size, 1) -> false;
is_gc_bif(Name, Arity) when is_atom(Name), is_integer(Arity), Arity >= 0 ->
    not (erl_internal:bool_op(Name, Arity) orelse
	 erl_internal:new_type_test(Name, Arity) orelse
	 erl_internal:comp_op(Name, Arity)).
