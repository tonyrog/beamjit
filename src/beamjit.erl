%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Generate beam code JIT
%%% @end
%%% Created :  4 Jun 2022 by Tony Rogvall <tony@rogvall.se>

-module(beamjit).

%% -define(verbose(F,A), io:format((F),(A))).
-define(verbose(F,A), ok).

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
-export([make_fun2/0]).          %% 103*
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

%% debug
-export([load_file/1, load_binary/1]).
-export([print_chunks/1]).
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
	 lines     :: #jctx_table{map::jctx_map(line())}
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
	'P' |  %% u     byte offset
	'Q' |  %% u     unpackable byte offset
	h   |  %% u     character
	fr     %% float reg (fr)
	.
	
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

label(L) when is_integer(L), L>= 0 ->
    emit_op(?LABEL),
    emit_u(L).
    
func_info(Mod,Func,Arity) when is_atom(Mod), is_atom(Func), 
			       is_integer(Arity), Arity >= 0 ->
    emit_op(?FUNC_INFO),
    emit_a({atom,Mod}),
    emit_a({atom,Func}),
    emit_u(Arity).

int_code_end() ->
    emit_op(?INT_CODE_END).

call(Arity, F) ->
    emit_op(?CALL),
    emit_(Arity),
    emit_(F).

call_last(Arity, F, Dealloc) ->
    emit_op(?CALL_LAST),
    emit_(Arity),
    emit_(F),
    emit_(Dealloc).

call_only(Arity,F) ->
    emit_op(?CALL_ONLY),
    emit_(Arity),
    emit_(F).

call_ext(Arity,Ext) ->
    emit_op(?CALL_EXT),
    emit_(Arity),
    emit_(Ext).

call_ext_last(Arity,Ext,Dealloc) ->
    emit_op(?CALL_EXT_LAST),
    emit_(Arity),
    emit_(Ext),
    emit_(Dealloc).

bif0(Bif, Dst) ->
    emit_op(?BIF0,[{extfunc,erlang,Bif,0},Dst]).
    
bif1(Fail, Bif, A1, Dst) ->
    case is_bif(Bif, 1) of
	false -> error({Bif, not_a_bif});
	true ->
	    case is_gc_bif(Bif, 1) of
		true ->
		    gc_bif1(Fail,live(),Bif,A1,Dst);
		false ->
		    emit_op(?BIF1,[Fail,{extfunc,erlang,Bif,1},A1,Dst]) 
	    end
    end.

bif2(Fail, Bif, A1, A2, Dst) ->
    case is_bif(Bif, 2) of
	false -> error({Bif, not_a_bif});
	true ->
	    case is_gc_bif(Bif, 2) of
		true ->
		    gc_bif2(Fail,live(),Bif,A1,A2,Dst);
		false ->
		    emit_op(?BIF2,[Fail,{extfunc,erlang,Bif,2},A1,A2,Dst]) 
	    end
    end.

bif3(Fail, Bif, A1, A2, A3, Dst) ->
    case is_bif(Bif, 3) of
	false -> error({Bif, not_a_bif});
	true ->
	    case is_gc_bif(Bif, 3) of
		true ->
		    gc_bif3(Fail,live(),Bif,A1,A2,A3,Dst)
		%% there is no BIF3 yet..
	    end
    end.

allocate(StackNeed) ->
    allocate(StackNeed, live()).
allocate(StackNeed,Live) ->
    emit_op(?ALLOCATE),
    emit_(StackNeed),
    emit_(Live).

allocate_heap(StackNeed,HeapNeed) ->
    allocate_heap(StackNeed,HeapNeed,live()).
allocate_heap(StackNeed,HeapNeed,Live) ->
    emit_op(?ALLOCATE_HEAP),
    emit_(StackNeed),
    emit_(HeapNeed),
    emit_(Live).
    
allocate_zero(StackNeed) ->
    allocate_zero(StackNeed,live()).
allocate_zero(StackNeed,Live) ->
    emit_op(?ALLOCATE_ZERO),
    emit_(StackNeed),
    emit_(Live).

allocate_heap_zero(StackNeed,HeapNeed) ->
    allocate_heap_zero(StackNeed,HeapNeed,live()).
allocate_heap_zero(StackNeed,HeapNeed,Live) ->
    emit_op(?ALLOCATE_HEAP_ZERO),
    emit_(StackNeed),
    emit_(HeapNeed),
    emit_(Live).

test_heap(HeapNeed) ->
    test_heap(HeapNeed,live()).
test_heap(HeapNeed,Live) ->
    emit_op(?TEST_HEAP),
    emit_(HeapNeed),
    emit_(Live).
    
init(Dst) ->
    emit_op(?INIT),
    emit_(Dst).

deallocate(Deallocate) ->
    emit_op(?DEALLOCATE),
    emit_(Deallocate).

return() ->
    emit_op(?RETURN).

send() ->
    emit_op(?SEND).

remove_message() ->
    emit_op(?REMOVE_MESSAGE).

timeout() ->
    emit_op(?TIMEOUT).

loop_rec(F,Dst) ->
    emit_op(?LOOP_REC),
    emit_(F),
    emit_(Dst).
    
loop_rec_end(F) ->
    emit_op(?LOOP_REC_END),
    emit_(F).

wait(F) ->
    emit_op(?WAIT),
    emit_(F).

wait_timeout(F,Src) ->
    emit_op(?WAIT_TIMEOUT),
    emit_(F),
    emit_(Src).
    
m_plus(Fail,A1,A2,Reg) -> 
    emit_op(?M_PLUS),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).
    
m_minus(Fail,A1,A2,Reg) ->
    emit_op(?M_MINUS),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

m_times(Fail,A1,A2,Reg) ->
    emit_op(?M_TIMES),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

m_div(Fail,A1,A2,Reg) ->
    emit_op(?M_DIV),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

int_div(Fail,A1,A2,Reg) ->
    emit_op(?INT_DIV),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

int_rem(Fail,A1,A2,Reg) ->
    emit_op(?INT_REM),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

int_band(Fail,A1,A2,Reg) ->
    emit_op(?INT_BAND),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

int_bor(Fail,A1,A2,Reg) ->
    emit_op(?INT_BOR),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

int_bxor(Fail,A1,A2,Reg) ->
    emit_op(?INT_BXOR),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

int_bsl(Fail,A1,A2,Reg) ->
    emit_op(?INT_BSL),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

int_bsr(Fail,A1,A2,Reg) ->
    emit_op(?INT_BSR),
    emit_(Fail), emit_(A1), emit_(A2),  emit_(Reg).

int_bnot(Fail,A1,Reg) ->
    emit_op(?INT_BNOT),
    emit_(Fail), emit_(A1), emit_(Reg).

is_lt(Fail,A1,A2) -> 
    emit_op(?IS_LT),
    emit_(Fail), emit_(A1), emit_(A2).

is_ge(Fail,A1,A2) ->
    emit_op(?IS_GE),
    emit_(Fail), emit_(A1), emit_(A2).

is_eq(Fail,A1,A2) ->
    emit_op(?IS_EQ),
    emit_(Fail), emit_(A1), emit_(A2).

is_ne(Fail,A1,A2) ->
    emit_op(?IS_NE),
    emit_(Fail), emit_(A1), emit_(A2).

is_eq_exact(Fail,A1,A2) ->
    emit_op(?IS_EQ_EXACT),
    emit_(Fail), emit_(A1), emit_(A2).

is_ne_exact(Fail,A1,A2) ->
    emit_op(?IS_NE_EXACT),
    emit_(Fail), emit_(A1), emit_(A2).

is_integer(Fail,A1) ->
    emit_op(?IS_INTEGER),
    emit_(Fail), emit_(A1).

is_float(Fail,A1) ->
    emit_op(?IS_FLOAT),
    emit_(Fail), emit_(A1).

is_number(Fail,A1) ->
    emit_op(?IS_NUMBER),
    emit_(Fail), emit_(A1).

is_atom(Fail,A1) ->
    emit_op(?IS_ATOM),
    emit_(Fail), emit_(A1).

is_pid(Fail,A1) ->
    emit_op(?IS_PID),
    emit_(Fail), emit_(A1).

is_reference(Fail,A1) ->
    emit_op(?IS_REFERENCE),
    emit_(Fail), emit_(A1).

is_port(Fail,A1) ->
    emit_op(?IS_PORT),
    emit_(Fail), emit_(A1).

is_nil(Fail,A1) ->
    emit_op(?IS_NIL),
    emit_(Fail), emit_(A1).

is_binary(Fail,A1) ->
    emit_op(?IS_BINARY),
    emit_(Fail), emit_(A1).

is_constant(Fail,A1) ->
    emit_op(?IS_CONSTANT),
    emit_(Fail), emit_(A1).

is_list(Fail,A1) ->
    emit_op(?IS_LIST),
    emit_(Fail), emit_(A1).

is_nonempty_list(Fail,A1) ->
    emit_op(?IS_NONEMPTY_LIST),
    emit_(Fail), emit_(A1).

is_tuple(Fail,A1) ->
    emit_op(?IS_TUPLE),
    emit_(Fail), emit_(A1).

test_arity(Fail,Src,Size) ->
    emit_op(?TEST_ARITY),
    emit_(Fail), emit_(Src), emit_(Size).

select_val(Val,Fail,Pairs) ->
    emit_op(?SELECT_VAL),
    emit_(Val), emit_(Fail), emit_(Pairs).

select_tuple_arity(Val,Fail,Pairs) ->
    emit_op(?SELECT_TUPLE_ARITY),
    emit_(Val), emit_(Fail), emit_(Pairs).

jump(F) ->
    emit_op(?JUMP),
    emit_(F).

'catch'(Dst,Fail) ->
    emit_op(?CATCH),
    emit_(Dst), emit_(Fail).

catch_end(Dst) ->
    emit_op(?CATCH_END),
    emit_(Dst).

move(Src,Dst) ->
    emit_op(?MOVE),
    emit_s(Src), emit_d(Dst).

get_list(Src,Head,Tail) ->
    emit_op(?GET_LIST),
    emit_(Src), emit_(Head), emit_(Tail).
     
get_tuple_element(Src,Ix,Dst) ->
    emit_op(?GET_TUPLE_ELEMENT),
    emit_(Src), emit_(Ix), emit_(Dst).

set_tuple_element(Val,Dst,Ix) ->
    emit_op(?SET_TUPLE_ELEMENT),
    emit_(Val), emit_(Dst), emit_(Ix).

put_string(Len,String,Dst) ->
    emit_op(?PUT_STRING),
    emit_(Len), emit_(String), emit_(Dst).

put_list(Head,Tail,Dst) ->
    emit_op(?PUT_LIST),
    emit_(Head), emit_(Tail), emit_(Dst).

put_tuple(Arity,Dst) ->
    emit_op(?PUT_TUPLE),
    emit_(Arity), emit_(Dst).

put(Src) ->
    emit_op(?PUT),
    emit_(Src).

badmatch(Fail) ->
    emit_op(?BADMATCH),
    emit_(Fail).

if_end() ->
    emit_op(?IF_END).

case_end(CaseVal) ->
    emit_op(?CASE_END),
    emit_(CaseVal).

call_fun(Arity) ->
    emit_op(?CALL_FUN),
    emit_(Arity).

make_fun(Arg1, Arg2, Arg3) ->
    emit_op(?MAKE_FUN),
    emit_(Arg1), emit_(Arg2), emit_(Arg3).
    
is_function(Fail,A1) ->
    emit_op(?IS_FUNCTION),
    emit_(Fail), emit_(A1).

call_ext_only(Arity,Fun) ->
    emit_op(?CALL_EXT_ONLY),
    emit_(Arity), emit_(Fun).

bs_start_match(Fail,Reg) ->
    emit_op(?BS_START_MATCH),
    emit_(Fail), emit_(Reg).
    
bs_get_integer(Fail,List) ->
    emit_op(?BS_GET_INTEGER),
    emit_(Fail), emit_(List).
    
bs_get_float(Fail,List) ->
    emit_op(?BS_GET_FLOAT),
    emit_(Fail), emit_(List).
    
bs_get_binary(Fail,List) ->
    emit_op(?BS_GET_BINARY),
    emit_(Fail), emit_(List).

bs_skip_bits(Fail,List) ->
    emit_op(?BS_SKIP_BITS),
    emit_(Fail), emit_(List).

bs_test_tail(Fail,List) ->
    emit_op(?BS_TEST_TAIL),
    emit_(Fail), emit_(List).

bs_save(N) ->
    emit_op(?BS_SAVE),
    emit_(N).

bs_restore(N) ->
    emit_op(?BS_RESTORE),
    emit_(N).
    
bs_init(N,Flags) ->
    emit_op(?BS_INIT),
    emit_(N), emit_(Flags).

bs_final(Fail,X) ->
    emit_op(?BS_FINAL),
    emit_(Fail), emit_(X).

bs_put_integer(Fail,ArgSz,N,Flags,ArgInt) ->
    emit_op(?BS_PUT_INTEGER),
    emit_(Fail), emit_(ArgSz), emit_(N), emit_(Flags), emit_(ArgInt).
    
bs_put_binary(Fail,ArgSz,N,Flags,ArgBin) ->
    emit_op(?BS_PUT_BINARY),
    emit_(Fail), emit_(ArgSz), emit_(N), emit_(Flags), emit_(ArgBin).

bs_put_float(Fail,ArgSz,N,Flags,ArgFloat) ->
    emit_op(?BS_PUT_FLOAT),
    emit_(Fail), emit_(ArgSz), emit_(N), emit_(Flags), emit_(ArgFloat).

bs_put_string(Len,StrArg) ->
    emit_op(?BS_PUT_STRING),
    emit_(Len), emit_(StrArg).

bs_need_buf(N) ->
    emit_op(?BS_NEED_BUF),
    emit_(N).

fclearerror() ->
    emit_op(?FCLEARERROR).

fcheckerror(Fail) ->
    emit_op(?FCHECKERROR),
    emit_(Fail).

fmove(Src,FDst) ->
    emit_op(?FMOVE),
    emit_(Src), emit_(FDst).

fconv(Src,FDst) ->
    emit_op(?FCONV),
    emit_(Src), emit_(FDst).

fadd(Fail,FA1,FA2,FDst) ->
    emit_op(?FADD),
    emit_(Fail), emit_(FA1), emit_(FA2), emit_(FDst).

fsub(Fail,FA1,FA2,FDst) ->
    emit_op(?FSUB),
    emit_(Fail), emit_(FA1), emit_(FA2), emit_(FDst).

fmul(Fail,FA1,FA2,FDst) ->
    emit_op(?FMUL),
    emit_(Fail), emit_(FA1), emit_(FA2), emit_(FDst).

fdiv(Fail,FA1,FA2,FDst) ->
    emit_op(?FDIV),
    emit_(Fail), emit_(FA1), emit_(FA2), emit_(FDst).

fnegate(Fail,FA1,FDst) ->
    emit_op(?FNEGATE),
    emit_(Fail), emit_(FA1), emit_(FDst).

make_fun2() ->
    emit_op(?MAKE_FUN2).

'try'(Reg,Fail) ->
    emit_op(?TRY),
    emit_(Reg), emit_(Fail).

try_end(Reg) ->
    emit_op(?TRY_END),
    emit_(Reg).

try_case(Reg) ->
    emit_op(?TRY_CASE),
    emit_(Reg).

try_case_end(TryVal) ->
    emit_op(?TRY_CASE_END),
    emit_(TryVal).

raise(Class,Reason) ->
    emit_op(?RAISE),
    emit_(Class), emit_(Reason).

bs_init2(Fail,Src,W,R,Flags,Dst) ->
    emit_op(?BS_INIT2),
    emit_(Fail),emit_(Src),emit_(W),emit_(R),emit_(Flags),emit_(Dst).

bs_bits_to_bytes(Fail,Src,Dst) ->
    emit_op(?BS_BITS_TO_BYTES),
    emit_(Fail), emit_(Src),emit_(Dst).

bs_add(Fail,Src1,Src2,Unit,Dst) ->
    emit_op(?BS_ADD),
    emit_(Fail), emit_(Src1), emit_(Src2), emit_(Unit), emit_(Dst).

apply(Arity) ->
    emit_op(?APPLY),
    emit_(Arity).

apply_last(Arity,U) ->
    emit_op(?APPLY_LAST),
    emit_(Arity), emit_(U).

is_boolean(Fail,A1) ->
    emit_op(?IS_BOOLEAN),
    emit_(Fail), emit_(A1).

is_function2(Fail,A1,A2) ->
    emit_op(?IS_FUNCTION2),
    emit_(Fail), emit_(A1), emit_(A2).


bs_start_match2(Fail,Ctx,Live,Save,Dst) ->
    emit_op(?BS_START_MATCH2),
    emit_(Fail), emit_(Ctx), emit_(Live), emit_op(Save), emit_(Dst).

bs_get_integer2(Fail,Ctx,Live,Size,N,Flags,Dst) ->
    emit_op(?BS_GET_INTEGER2),
    emit_(Fail), emit_(Ctx), emit_(Live), emit_(Size), emit_op(N), 
    emit_(Flags), emit_(Dst).

bs_get_float2(Fail,Ctx,Live,Size,N,Flags,Dst) ->
    emit_op(?BS_GET_FLOAT2),
    emit_(Fail), emit_(Ctx), emit_(Live), emit_(Size), emit_op(N), 
    emit_(Flags), emit_(Dst).

bs_get_binary2(Fail,Ctx,Live,Size,N,Flags,Dst) ->
    emit_op(?BS_GET_BINARY2),
    emit_(Fail), emit_(Ctx), emit_(Live), emit_(Size), emit_op(N), 
    emit_(Flags), emit_(Dst).

bs_skip_bits2(Fail,Ctx,Size,Unit,Flags) ->
    emit_op(?BS_SKIP_BITS2),
    emit_(Fail), emit_(Ctx), emit_(Size), emit_op(Unit), emit_(Flags).

bs_test_tail2(Fail,Ctx,N) ->
    emit_op(?BS_TEST_TAIL2),
    emit_(Fail), emit_(Ctx), emit_(N).

bs_save2(Ctx, N) ->
    emit_op(?BS_SAVE2),
    emit_(Ctx), emit_(N).

bs_restore2(Ctx, N) ->
    emit_op(?BS_RESTORE2),
    emit_(Ctx), emit_(N).

gc_bif1(Fail,Live,Bif,A1,Dst) ->
    emit_op(?GC_BIF1),
    emit_(Fail), emit_(Live), emit_({extfunc,erlang,Bif,1}), 
    emit_(A1), emit_(Dst).

gc_bif2(Fail,Live,Bif,A1,A2,Dst) ->
    emit_op(?GC_BIF2),
    emit_(Fail), emit_(Live), emit_({extfunc,erlang,Bif,2}),
    emit_(A1), emit_(A2), emit_(Dst).

bs_final2(X,Y) ->
    emit_op(?BS_FINAL),
    emit_(X), emit_(Y).

bs_bits_to_bytes2(A2,A3) ->
    emit_op(?BS_BITS_TO_BYTES2),
    emit_(A2), emit_(A3).

put_literal(Index, Dst) ->
    emit_op(?PUT_LITERAL),
    emit_(Index), emit_(Dst).

is_bitstr(Fail,A1) ->
    emit_op(?IS_BITSTR),
    emit_(Fail), emit_(A1).

bs_context_to_binary(Dst) ->
    emit_op(?BS_CONTEXT_TO_BINARY),
    emit_(Dst).

bs_test_unit(Fail,Ctx,N) ->
    emit_op(?BS_TEST_UNIT),
    emit_(Fail), emit_(Ctx), emit_(N).

bs_match_string(Fail,Ctx,Bits,String) ->
    emit_op(?BS_MATCH_STRING),
    emit_(Fail), emit_(Ctx), emit_(Bits), emit_(String).

bs_init_writable() ->
    emit_op(?BS_INIT_WRITABLE).
    
bs_append(Fail,Arg2,W,R,U,Arg6,Flags,Arg8) ->
    emit_op(?BS_APPEND),
    emit_(Fail), emit_(Arg2), emit_(W), emit_(R), emit_(U),
    emit_(Arg6), emit_(Flags), emit_(Arg8).

bs_private_append(Fail,Arg2,U,Arg4,Flags,Arg6) ->
    emit_op(?BS_PRIVATE_APPEND),
    emit_(Fail), emit_(Arg2), emit_(U), emit_(Arg4), emit_(Flags), emit_(Arg6).

trim(N,Remaining) ->
    emit_op(?TRIM),
    emit_(N), emit_(Remaining).

bs_init_bits(Fail,Arg2,W,R,Flags,Arg6) ->
    emit_op(?BS_INIT_BITS),
    emit_(Fail), emit_(Arg2), emit_(W), emit_(R), emit_(Flags), emit_(Arg6).
    
bs_get_utf8(Fail,Arg2,Arg3,Flags,Arg4) ->
    emit_op(?BS_GET_UTF8),
    emit_(Fail), emit_(Arg2), emit_(Arg3), emit_(Flags), emit_(Arg4).

bs_skip_utf8(Fail,Arg2,Arg3,Flags) ->
    emit_op(?BS_SKIP_UTF8),
    emit_(Fail), emit_(Arg2), emit_(Arg3), emit_(Flags).

bs_get_utf16(Fail,Arg2,Arg3,Flags,Arg4) ->
    emit_op(?BS_GET_UTF16),
    emit_(Fail), emit_(Arg2), emit_(Arg3), emit_(Flags), emit_(Arg4).

bs_skip_utf16(Fail,Arg2,Arg3,Flags) ->
    emit_op(?BS_SKIP_UTF16),
    emit_(Fail), emit_(Arg2), emit_(Arg3), emit_(Flags).

bs_get_utf32(Fail,Arg2,Arg3,Flags,Arg4) ->
    emit_op(?BS_GET_UTF32),
    emit_(Fail), emit_(Arg2), emit_(Arg3), emit_(Flags), emit_(Arg4).

bs_skip_utf32(Fail,Arg2,Arg3,Flags) ->
    emit_op(?BS_SKIP_UTF32),
    emit_(Fail), emit_(Arg2), emit_(Arg3), emit_(Flags).

bs_utf8_size(Fail,Arg2,Arg3) ->
    emit_op(?BS_UTF8_SIZE),
    emit_(Fail), emit_(Arg2), emit_(Arg3).

bs_put_utf8(Fail,Flags,Arg3) ->
    emit_op(?BS_PUT_UTF8),
    emit_(Fail), emit_(Flags), emit_(Arg3).

bs_utf16_size(Fail,Arg2,Arg3) ->
    emit_op(?BS_UTF16_SIZE),
    emit_(Fail), emit_(Arg2), emit_(Arg3).

bs_put_utf16(Fail,Flags,Arg3) ->
    emit_op(?BS_PUT_UTF16),
    emit_(Fail), emit_(Flags), emit_(Arg3).

bs_put_utf32(Fail,Flags,Arg3) ->
    emit_op(?BS_PUT_UTF32),
    emit_(Fail), emit_(Flags), emit_(Arg3).

on_load() ->
    emit_op(?ON_LOAD).

recv_mark(F) ->
    emit_op(?RECV_MARK),
    emit_(F).

recv_set(F) ->
    emit_op(?RECV_SET),
    emit_(F).
    
gc_bif3(Fail,Live,Bif,A1,A2,A3,Dst) ->
    emit_op(?GC_BIF3),
    emit_(Fail),emit_(Live),emit_({extfunc,erlang,Bif,3}),
    emit_(A1),emit_(A2),emit_(A3),emit_(Dst).

line(Line) ->
    inc_num_lines(),
    emit_op(?LLINE),
    emit_L(Line).

put_map_assoc(A1,A2,A3,A4,A5) ->
    emit_op(?PUT_MAP_ASSOC),
    emit_(A1),emit_(A2),emit_(A3),emit_(A4),emit_(A5).

put_map_exact(A1,A2,A3,A4,A5) ->
    emit_op(?PUT_MAP_EXACT),
    emit_(A1),emit_(A2),emit_(A3),emit_(A4),emit_(A5).

is_map(A1,A2) ->
    emit_op(?IS_MAP),
    emit_(A1),emit_(A2).

has_map_fields(A1,A2,A3) ->
    emit_op(?HAS_MAP_FIELDS),
    emit_(A1),emit_(A2),emit_(A3).

get_map_elements(A1,A2,A3) ->
    emit_op(?GET_MAP_ELEMENTS),
    emit_(A1),emit_(A2),emit_(A3).

is_tagged_tuple(A1,A2,A3,A4) ->
    emit_op(?IS_TAGGED_TUPLE),
    emit_(A1),emit_(A2),emit_(A3),emit_(A4).
    
build_stacktrace() ->
    emit_op(?BUILD_STACKTRACE).

raw_raise() ->
    emit_op(?RAW_RAISE).

get_hd(A1,A2) ->
    emit_op(?GET_HD),
    emit_(A1),emit_(A2).
    
get_tl(A1,A2) ->
    emit_op(?GET_TL),
    emit_(A1),emit_(A2).

put_tuple2(A1,A2) ->
    emit_op(?PUT_TUPLE2),
    emit_(A1),emit_(A2).

bs_get_tail(A1,A2,A3) ->
    emit_op(?BS_GET_TAIL),
    emit_(A1),emit_(A2),emit_(A3).

bs_start_match3(A1,A2,A3,A4) ->
    emit_op(?BS_START_MATCH3),
    emit_(A1),emit_(A2),emit_(A3),emit_(A4).

bs_get_position(A1,A2,A3) ->
    emit_op(?BS_GET_POSITION),
    emit_(A1),emit_(A2),emit_(A3).

bs_set_position(A1,A2) ->
    emit_op(?BS_SET_POSITION),
    emit_(A1),emit_(A2).

swap(A1,A2) ->
    emit_op(?SWAP),
    emit_(A1),emit_(A2).

bs_start_match4(A1,A2,A3,A4) ->
    emit_op(?BS_START_MATCH4),
    emit_(A1),emit_(A2),emit_(A3),emit_(A4).

make_fun3(A1,A2,A3) ->
    emit_op(?MAKE_FUN3),
    emit_(A1),emit_(A2),emit_(A3).
    
init_yregs(A1) ->
    emit_op(?INIT_YREGS),
    emit_(A1).

recv_marker_bind(A1,A2) ->
    emit_op(?RECV_MARKER_BIND),
    emit_(A1), emit_(A2).

recv_marker_clear(A1) ->
    emit_op(?RECV_MARKER_CLEAR),
    emit_(A1).
    
recv_marker_reserve(A1) ->
    emit_op(?RECV_MARKER_RESERVE),
    emit_(A1).

recv_marker_use(A1) ->
    emit_op(?RECV_MARKER_USE),
    emit_(A1).

bs_create_bin(Fail,Alloc,Unit,Dst,OpList) ->
    bs_create_bin(Fail,Alloc,live(),Unit,Dst,OpList).

bs_create_bin(Fail,Alloc,Live,Unit,Dst,OpList) ->
    emit_op(?BS_CREATE_BIN),
    emit_F(Fail), emit_(Alloc), emit_(Live), 
    emit_(Unit), emit_(Dst), emit_(OpList).

call_fun2(Tag, Arity, Func) ->
    emit_op(?CALL_FUN2),
    emit_(Tag), emit_A(Arity), emit_(Func).

nif_start() ->
    emit_op(?NIF_START).

badrecord(Value) ->
    emit_op(?BADRECORD),
    emit_(Value).

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
	     lines = #jctx_table{index=1}
	    },
    jctx(Ctx),
    ok.

jterminate_() ->
    Ctx = jctx(),
    {ok,Bin} = ram_file:get_file(Ctx#jctx.fd),
    ram_file:close(Ctx#jctx.fd),
    erase('JCTX'),
    Bin.

emit_op(Opcode) when ?is_byte(Opcode) ->
    ?verbose("opcode = ~p\n", [Opcode]),
    Ctx = jctx(),
    MaxOp = max(Ctx#jctx.max_op, Opcode),
    jctx(Ctx#jctx { max_op = MaxOp }),
    emit_data([Opcode]).

emit_op(Opcode, Args) when ?is_byte(Opcode), is_list(Args) ->
    Ctx = jctx(),
    MaxOp = max(Ctx#jctx.max_op, Opcode),
    jctx(Ctx#jctx { max_op = MaxOp }),
    #opcode{argtypes=As} = maps:get(Opcode, opcode_map()),
    emit_data([Opcode]),
    emit_arglist(As, Args).

emit_arglist([A|As], [Arg|Args]) ->
    emit_arg(A, Arg),
    emit_arglist(As, Args);
emit_arglist([], []) ->
    ok.

emit_arg(A, Arg) ->
    case A of
	s -> emit_s(Arg);
	d -> emit_d(Arg);
	u -> emit_u(Arg);
	i -> emit_i(Arg);
	r -> emit_x(Arg);  %% fixme check {x,0}
	x -> emit_x(Arg);
	y -> emit_y(Arg);
	a -> emit_a(Arg);
	j -> emit_j(Arg);
	fr -> emit_fr(Arg);
	'A' -> emit_A(Arg);
	'F' -> emit_F(Arg);
	'U' -> emit_u(Arg);
	'G' -> emit_G(Arg);
	'L' -> emit_L(Arg);
	'_' -> emit_(Arg)
    end.    
    
%% s = x|y|literal
emit_s({x,X}) when ?is_reg(X) ->
    emit_data(encode_ival(?X,X));
emit_s({y,Y})  when ?is_reg(Y) ->
    emit_data(encode_ival(?Y,Y));
emit_s({literal,L}) ->
    emit_data(encode_literal(L)).

%% d = x|y
emit_d({x,X}) when ?is_reg(X) ->
    emit_data(encode_ival(?X,X));
emit_d({y,Y})  when ?is_reg(Y) ->
    emit_data(encode_ival(?Y,Y)).

emit_u({u,U}) when is_integer(U), U >= 0 ->
    emit_data(encode_ival(?U, U));
emit_u(U)  when is_integer(U), U >= 0 -> %% {label,L}
    emit_data(encode_ival(?U, U)).

emit_i({i,I}) when is_integer(I) ->
    emit_data(encode_ival(?I, I)).

emit_a({atom,A}) when is_atom(A) ->
    emit_data(encode_atom(A));
emit_a({a,I}) when is_integer(I), I >= 1 ->
    emit_data(encode_ival(?A, I)).

emit_x({x,X}) when ?is_reg(X) ->
    emit_data(encode_ival(?X,X)).

emit_y({y,Y})  when ?is_reg(Y) ->
    emit_data(encode_ival(?Y,Y)).

emit_j({f,F}) ->
    emit_data(encode_ival(?F,F)).

emit_fr(Arg={fr,_}) ->
    emit_data(encode_(Arg)).

emit_F(Arg) ->
    emit_data(encode_import(Arg)).

emit_A({u,A}) when A >= 0 -> %% arity value
    emit_data(encode_ival(?U, A));
emit_A(A) when is_integer(A), A>=0 ->
    emit_data(encode_ival(?U, A)).

emit_G({u,U}) when is_integer(U) ->
    emit_data(encode_ival(?U, U));
emit_G(Flags) when is_list(Flags) ->
    U = encode_bit_flags(Flags),
    emit_data(encode_ival(?U, U)).

emit_L([]) ->
    emit_data(encode_ival(?U, 0));
emit_L(Line=[{location,_Fname,_Ln}]) ->
    emit_data(encode_line(Line)).


encode_bit_flags([little|Fs]) ->
    ?BIT_LITTLE bor encode_bit_flags(Fs);
encode_bit_flags([big|Fs]) ->
    ?BIT_BIG bor encode_bit_flags(Fs);
encode_bit_flags([nativ|Fs]) ->
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
    if U band ?BIT_LITTLE =/= 0 -> [little]; true -> [] end ++
    if U band ?BIT_SIGNED =/= 0 -> [signed]; true -> [] end ++
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
encode_({fr,R})  ->
    iolist_to_binary([encode_ival(?Z, ?FR),
		      encode_ival(?U, R)]);
encode_({list,L}) ->
    {N,Data} = encode_list(L),
    iolist_to_binary([encode_ival(?Z, ?LIST),
		      encode_ival(?U, N),
		      Data]);
encode_({alloc,L}) ->
    {N,Data} = encode_alloc_list(L),
    iolist_to_binary([encode_ival(?Z, ?ALLOC),
		      encode_ival(?U, N),
		      Data]);
encode_({literal,L}) -> %% inline literal
    Data = term_to_binary(L),
    N = byte_size(Data),
    iolist_to_binary([encode_ival(?Z, ?LITERAL),
		      encode_ival(?U, N),
		      Data]);
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
    {{u,N},Bin1} = decode_ival(Bin),
    {decode_literal(N),Bin1};
decode(<<?FR:4,0:1,?Z:3, Bin/binary>>) ->
    {{u,R},Bin1} = decode(Bin),
    {{fr,R}, Bin1};
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
		{{a,A},Bin1} ->
		    decode_arglist(Ts,Bin1,[decode_atom(A) | Acc]);
		{A={atom,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[A | Acc]);
		{X={x,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[X | Acc]);
		{Y={y,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Y | Acc])
	    end;
	d ->
	    case decode(Bin) of
		{X={x,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[X | Acc]);
		{Y={y,_},Bin1} ->
		    decode_arglist(Ts,Bin1,[Y | Acc])
	    end;
	fr ->
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

load_file(Filename) ->
    jinit_(),
    Loaded = beamjit_file:fold_chunks(Filename, fun load_chunk/3, []),
    RChunks = resolve_chunks(Loaded, []),
    jterminate_(),
    {ok, RChunks}.

load_binary(Binary) ->
    jinit_(),
    Loaded = beamjit_file:fold_chunks(Binary, fun load_chunk/3, []),
    RChunks = resolve_chunks(Loaded, []),
    jterminate_(),
    %% {ok, Loaded}.
    {ok, RChunks}.

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
    {NL,NF,Line} = build_lines(),
    NumLineInstrs = Ctx#jctx.num_lines,
    LineChunk = <<?LINES_VER:32,?LINES_BITS:32,
		  NumLineInstrs:32,
		  (NL-1):32,NF:32,Line/binary>>,
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


print_chunks({ok,Chunks}) ->
    print_chunks(Chunks);
print_chunks([{code,Opts,InstrList}|Chunks]) ->
    io:format("*** CODE ***\n"),
    print_functions(InstrList),
    io:format("*** CODEINFO ***\n"),
    lists:foreach(
      fun({K,V}) ->
	      io:format("  ~w: ~p\n", [K,V])
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

%% pass2 resolve code and reverse all chunks 
resolve_chunks([{code,Info,Code}|Chunks], Acc) ->
    {Asm,OpRangeSet} = decode_opcodes(Code),
    All = gb_sets:from_list(lists:seq(1,?MAX_OP_CODE)),
    OpRangeNotUsed0 = gb_sets:difference(All,OpRangeSet),
    OpRangeNotUsed  = gb_sets:difference(OpRangeNotUsed0, obsolete()),
    Info1 = [{opcode_range_num,gb_sets:size(OpRangeSet)},
	     {opcode_range_set,gb_sets_to_range_list(OpRangeSet)},
	     {opcode_range_not_used,gb_sets_to_range_list(OpRangeNotUsed)}
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
    build_lines_(LineList, 0, FnMap, []).

build_lines_([{_I,{Fname,Ln}}|List], J, FnMap, Acc) ->
    case maps:get(Fname, FnMap) of
	J -> 
	    build_lines_(List, J, FnMap,
			[encode_ival(?I,Ln)|Acc]);
	J1 ->
	    build_lines_(List, J1, FnMap,
			[encode_ival(?I,Ln),encode_ival(?A,J1)|Acc])
    end;
build_lines_([_|List], J, FnMap, Acc) ->
    build_lines_(List, J, FnMap, Acc);
build_lines_([], _J, FnMap, Acc) ->
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
    {NL,NF,Line}.

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

decode_opcodes(Binary) ->
    decode_opcodes(Binary, gb_sets:new(), []).

decode_opcodes(<<?INT_CODE_END, _Bin/binary>>, OpSet, Acc) ->
    %% ?verbose("trail bin=~p\n", [_Bin]),
    {lists:reverse(Acc), gb_sets:add_element(?INT_CODE_END, OpSet)};
decode_opcodes(<<Opcode,Bin/binary>>, OpSet, Acc) ->
    #opcode{mnemonic=Mnemonic,argtypes=As} = maps:get(Opcode, opcode_map()),
    ?verbose("opcode ~p, argtypes~p\n", [Mnemonic, As]),
    case decode_arglist(As,Bin) of
	{[],Bin1} ->
	    decode_opcodes(Bin1, gb_sets:add_element(Opcode, OpSet),
			   [Mnemonic | Acc]);
	{Args,Bin1} ->
	    ?verbose(" ~p\n", [Args]),
	    if Opcode =:= ?LLINE ->
		    inc_num_lines();
	       true ->
		    ok
	    end,
	    decode_opcodes(Bin1,gb_sets:add_element(Opcode,OpSet),
			   [list_to_tuple([Mnemonic|Args]) | Acc])
    end;
decode_opcodes(<<>>, OpSet, Acc) ->
    {lists:reverse(Acc), OpSet}.

inc_num_lines() ->
    Ctx = jctx(),
    NL = Ctx#jctx.num_lines + 1,
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


opcode_map() ->
#{
?OPENT(1,label,['U']),
?OPENT(2,func_info,[a,a,'U']),
?OPENT(3,int_code_end,[]),
?OPENT(4,call,['A',j]),
?OPENT(5,call_last,['A',j,'U']),
?OPENT(6,call_only,['A',j]),
?OPENT(7,call_ext,['A','F']),
?OPENT(8,call_ext_last,['A','F','U']),
?OPENT(9,bif0,['F','_']),
?OPENT(10,bif1,[j,'F','_','_']),
?OPENT(11,bif2,[j,'F','_','_','_']),
?OPENT(12,allocate,['U','U']),
?OPENT(13,allocate_heap,['U',k,'U']),
?OPENT(14,allocate_zero,['U','U']),
?OPENT(15,allocate_heap_zero,['U','U','U']),
?OPENT(16,test_heap,[k,'U']),
?OPENT(17,init,['_']),
?OPENT(18,deallocate,['U']),
?OPENT(19,return,[]),
?OPENT(20,send,[]),
?OPENT(21,remove_message,[]),
?OPENT(22,timeout,[]),
?OPENT(23,loop_rec,[j,d]),
?OPENT(24,loop_rec_end,[j]),
?OPENT(25,wait,[j]),
?OPENT(26,wait_timeout,[j,s]),
?OPENT(27,m_plus,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(28,m_minus,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(29,m_times,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(30,m_div,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(31,int_div,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(32,int_rem,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(33,int_band,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(34,int_bor,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(35,int_bxor,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(36,int_bsl,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(37,int_bsr,[j,s,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(38,int_bnot,[j,s,d],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(39,is_lt,[j,s,s]),
?OPENT(40,is_ge,[j,s,s]),
?OPENT(41,is_eq,[j,s,s]),
?OPENT(42,is_ne,[j,s,s]),
?OPENT(43,is_eq_exact,[j,s,s]),
?OPENT(44,is_ne_exact,[j,s,s]),
?OPENT(45,is_integer,[j,s]),
?OPENT(46,is_float,[j,s]),
?OPENT(47,is_number,[j,s]),
?OPENT(48,is_atom,[j,s]),
?OPENT(49,is_pid,[j,s]),
?OPENT(50,is_reference,[j,s]),
?OPENT(51,is_port,[j,s]),
?OPENT(52,is_nil,[j,s]),
?OPENT(53,is_binary,[j,s]),
?OPENT(54,is_constant,[j,s], ?OTP_R4, ?OTP_R13B_03),
?OPENT(55,is_list,[j,s]),
?OPENT(56,is_nonempty_list,[j,s]),
?OPENT(57,is_tuple,[j,s]),
?OPENT(58,test_arity,[j,s,'A']),
?OPENT(59,select_val,['_','_','_']),
?OPENT(60,select_tuple_arity,['_','_','_']),
?OPENT(61,jump,['_']),
?OPENT(62,'catch',['_','_']),
?OPENT(63,catch_end,['_']),
?OPENT(64,move,[s,d]),
?OPENT(65,get_list,['_','_','_']),
?OPENT(66,get_tuple_element,['_','_','_']),
?OPENT(67,set_tuple_element,['_','_','_']),
?OPENT(68,put_string,['_','_','_'],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(69,put_list,['_','_','_']),
?OPENT(70,put_tuple,['_','_'],?DEFAULT_VSN,?OTP_22),
?OPENT(71,put,['_'],?DEFAULT_VSN,?OTP_22),
?OPENT(72,badmatch,['_']),
?OPENT(73,if_end,[]),
?OPENT(74,case_end,['_']),
?OPENT(75,call_fun,['_']),
?OPENT(76,make_fun,['_','_','_'],?DEFAULT_VSN,?OTP_R13B_03),
?OPENT(77,is_function,['_','_']),
?OPENT(78,call_ext_only,['A','F'],?OTP_R5, ?OTP_R13B_03),
?OPENT(79,bs_start_match,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(80,bs_get_integer,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(81,bs_get_float,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(82,bs_get_binary,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(83,bs_skip_bits,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(84,bs_test_tail,['_','_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(85,bs_save,['_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(86,bs_restore,['_'],?OTP_R7, ?OTP_R13B_03),
?OPENT(87,bs_init,['_','_'],?OTP_R7A, ?OTP_R13B_03),
?OPENT(88,bs_final,['_','_'],?OTP_R7A, ?OTP_R13B_03),
?OPENT(89,bs_put_integer,['_','_','_','G','_'],?OTP_R7A, ?OTP_R13B_03),
?OPENT(90,bs_put_binary,['_','_','_','G','_'],?OTP_R7A, ?OTP_R13B_03),
?OPENT(91,bs_put_float,['_','_','_','G','_'],?OTP_R7A, ?OTP_R13B_03),
?OPENT(92,bs_put_string,['_','_'],?OTP_R7A, ?OTP_R13B_03),
?OPENT(93,bs_need_buf,['_'],?OTP_R7B, ?OTP_R13B_03),
?OPENT(94,fclearerror,[],?OTP_R8,?OTP_24),
?OPENT(95,fcheckerror,['_'],?OTP_R8,?OTP_24),
?OPENT(96,fmove,['_','_'],?OTP_R8),
?OPENT(97,fconv,['_','_'],?OTP_R8),
?OPENT(98,fadd,[j,fr,fr,fr],?OTP_R8),
?OPENT(99,fsub,[j,fr,fr,fr],?OTP_R8),
?OPENT(100,fmul,[j,fr,fr,fr],?OTP_R8),
?OPENT(101,fdiv,[j,fr,fr,fr],?OTP_R8),
?OPENT(102,fnegate,[j,fr,fr],?OTP_R8),
?OPENT(103,make_fun2,[],?OTP_R8,?OTP_24),
?OPENT(104,'try',['_','_'],?OTP_R10B),
?OPENT(105,try_end,['_'],?OTP_R10B),
?OPENT(106,try_case,['_'],?OTP_R10B),
?OPENT(107,try_case_end,['_'],?OTP_R10B),
?OPENT(108,raise,['_','_'],?OTP_R10B),
?OPENT(109,bs_init2,['_','_','_','_','G','_'],?OTP_R10B),
?OPENT(110,bs_bits_to_bytes,['_','_','_'],?OTP_R10B, ?OTP_R13B_03),
?OPENT(111,bs_add,['_','_','_','_','_'],?OTP_R10B),
?OPENT(112,apply,['_'],?OTP_R10B),
?OPENT(113,apply_last,['_','_'],?OTP_R10B),
?OPENT(114,is_boolean,['_','_'],?OTP_R10B),
?OPENT(115,is_function2,['_','_','_'],?OTP_R10B_6),
?OPENT(116,bs_start_match2,[j,'_','_','_','_'],?OTP_R11B,?OTP_22),
?OPENT(117,bs_get_integer2,[j,'_','_','_','_','G','_'],?OTP_R11B,?OTP_22),
?OPENT(118,bs_get_float2,[j,'_','_','_','_','G','_'],?OTP_R11B,?OTP_22),
?OPENT(119,bs_get_binary2,[j,'_','_','_','_','G','_'],?OTP_R11B,?OTP_22),
?OPENT(120,bs_skip_bits2,[j,'_','_','_','G'],?OTP_R11B,?OTP_22),
?OPENT(121,bs_test_tail2,[j,'_','_'],?OTP_R11B,?OTP_22),
?OPENT(122,bs_save2,['_','_'],?OTP_R11B,?OTP_22),
?OPENT(123,bs_restore2,['_','_'],?OTP_R11B,?OTP_22),
?OPENT(124,gc_bif1,[j,'U','F','_','_'],?OTP_R11B),
?OPENT(125,gc_bif2,[j,'U','F','_','_','_'],?OTP_R11B),
?OPENT(126,bs_final2,['_','_'],?OTP_R11B,?OTP_R12B),
?OPENT(127,bs_bits_to_bytes2,['_','_'],?OTP_R11B,?OTP_R12B),
?OPENT(128,put_literal,['_','_'],?OTP_R11B_4,?OTP_R12B),
?OPENT(129,is_bitstr,['_','_'],?OTP_R11B_5),
?OPENT(130,bs_context_to_binary,['_'],?OTP_R12B,?OTP_22),
?OPENT(131,bs_test_unit,[j,'_','_'],?OTP_R12B),
?OPENT(132,bs_match_string,['_','_','_','_'],?OTP_R12B),
?OPENT(133,bs_init_writable,[],?OTP_R12B),
?OPENT(134,bs_append,[j,'_','_','_','_','_','G','_'],?OTP_R12B),
?OPENT(135,bs_private_append,[j,'_','_','_','G','_'],?OTP_R12B),
?OPENT(136,trim,['U','U'],?OTP_R12B),
?OPENT(137,bs_init_bits,[j,'_','_','_','G','_'],?OTP_R12B),
?OPENT(138,bs_get_utf8,[j,'_','_','G','_'],?OTP_R12B_5),
?OPENT(139,bs_skip_utf8,[j,'_','_','G'],?OTP_R12B_5),
?OPENT(140,bs_get_utf16,[j,'_','_','G','_'],?OTP_R12B_5),
?OPENT(141,bs_skip_utf16,[j,'_','_','G'],?OTP_R12B_5),
?OPENT(142,bs_get_utf32,[j,'_','_','G','_'],?OTP_R12B_5),
?OPENT(143,bs_skip_utf32,[j,'_','_','G'],?OTP_R12B_5),
?OPENT(144,bs_utf8_size,[j,'_','_'],?OTP_R12B_5),
?OPENT(145,bs_put_utf8,[j,'G','_'],?OTP_R12B_5),
?OPENT(146,bs_utf16_size,[j,'_','_'],?OTP_R12B_5),
?OPENT(147,bs_put_utf16,[j,'G','_'],?OTP_R12B_5),
?OPENT(148,bs_put_utf32,[j,'G','_'],?OTP_R12B_5),
?OPENT(149,on_load,[],?OTP_R13B_03),
?OPENT(150,recv_mark,['_'],?OTP_R14A,?OTP_24),
?OPENT(151,recv_set,['_'],?OTP_R14A,?OTP_24),
?OPENT(152,gc_bif3,[j,'U','F','_','_','_','_'],?OTP_R14A),
?OPENT(153,line,['L'],?OTP_R15A),
?OPENT(154,put_map_assoc,['_','_','_','_','_'],?OTP_R17),
?OPENT(155,put_map_exact,['_','_','_','_','_'],?OTP_R17),
?OPENT(156,is_map,['_','_'],?OTP_R17),
?OPENT(157,has_map_fields,['_','_','_'],?OTP_R17),
?OPENT(158,get_map_elements,['_','_','_'],?OTP_R17),
?OPENT(159,is_tagged_tuple,['_','_','_','_'],?OTP_20),
?OPENT(160,build_stacktrace,[],?OTP_21),
?OPENT(161,raw_raise,[],?OTP_21),
?OPENT(162,get_hd,['_','_'],?OTP_21),
?OPENT(163,get_tl,['_','_'],?OTP_21),
?OPENT(164,put_tuple2,['_','_'],?OTP_22),
?OPENT(165,bs_get_tail,['_','_','_'],?OTP_22),
?OPENT(166,bs_start_match3,[j,'_','_','_'],?OTP_22),
?OPENT(167,bs_get_position,['_','_','_'],?OTP_22),
?OPENT(168,bs_set_position,['_','_'],?OTP_22),
?OPENT(169,swap,['_','_'],?OTP_23),
?OPENT(170,bs_start_match4,['_','_','_','_'],?OTP_23),
?OPENT(171,make_fun3,['_','_','_'],?OTP_24),
?OPENT(172,init_yregs,['_'],?OTP_24),
?OPENT(173,recv_marker_bind,['_','_'],?OTP_24),
?OPENT(174,recv_marker_clear,['_'],?OTP_24),
?OPENT(175,recv_marker_reserve,['_'],?OTP_24),
?OPENT(176,recv_marker_use,['_'],?OTP_24),
?OPENT(177,bs_create_bin,[j,'_','_','_','_','_'],?OTP_25),
?OPENT(178,call_fun2,['_','A',s],?OTP_25),
?OPENT(179,nif_start,[],?OTP_25),
?OPENT(180,badrecord,['_'],?OTP_25)
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
