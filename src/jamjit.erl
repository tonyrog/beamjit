%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Translate JAM code into BEAM 
%%% @end
%%% Created : 18 Jun 2022 by Tony Rogvall <tony@rogvall.se>

-module(jamjit).

-export([jit/1]).
-export([jbegin/0]).
-export([jend/0]).
-export([new_label/0]).
-export([bind/1]).
-export([exported_function/3, local_function/3]).
-export([module/1, source/1]).

-export([load_bin/1, code/1]).
-export([load_file/1, load_as_beam/1]).

-export([pushVar/1]).          %% 1..16,17
-export([storeVar/1]).         %% 18..33,34
-export([alloc/1]).            %% 35..50,51
-export([eqVar/1]).            %% 52..67,68
-export([eqArg/1]).            %% 69..84,85
-export([arg/1]).              %% 86..101,102
-export([unpkTuple/1]).        %% 103..118,119 
-export([mkTuple/1]).          %% 120..135,136
-export([getInt/1]).           %% 137..152,153,222
-export([hash/1]).             %% 154
-export([getFloat/1]).         %% 155
-export([pushInt/1]).          %% 156..171,172,221
-export([type/1]).             %% 173
-export([pushFloat/1]).        %% 174
-export([nodebug_info/3]).     %% 175
-export([list_length/0]).      %% 176
-export([getAtom/1]).          %% 177
-export([try_me_else_fail/0]). %% 178
-export([commit/0]).           %% 179
-export([ret/0]).              %% 180
-export([getNil/0]).           %% 181
-export([pushNil/0]).          %% 182
-export([pop/0]).              %% 183
-export([mkList/0]).           %% 184
-export([unpkList/0]).         %% 185
-export([send/0]).             %% 186
-export([self/0]).             %% 187
-export([pushStr/1]).          %% 188
-export([pushAtom/1]).         %% 189
-export([getStr/1]).           %% 190
-export([call_remote/3]).      %% 191
-export([head/0]).             %% 192
-export([enter_remote/3]).     %% 193
-export([dup/0]).              %% 194
-export([popCommit/0]).        %% 195
-export([failIf/0]).           %% 196
-export([failCase/0]).         %% 197
-export([tail/0]).             %% 198
-export([popCatch/0]).         %% 199
-export([setTimeout/0]).       %% 200
-export([wait/0]).             %% 201
-export([gotoix/2]).           %% 202
-export([popCommitJoin/0]).    %% 203
-export([save/0]).             %% 204
-export([arith_plus/0]).       %% 205
-export([arith_minus/0]).      %% 206
-export([arith_times/0]).      %% 207
-export([arith_div/0]).        %% 208
-export([arith_intdiv/0]).     %% 209
-export([arith_band/0]).       %% 210
-export([arith_bor/0]).        %% 211
-export([arith_bxor/0]).       %% 212
-export([arith_bnot/0]).       %% 213
-export([arith_bsl/0]).        %% 214
-export([arith_bsr/0]).        %% 215
-export([arith_rem/0]).        %% 216
-export([comp_gt/0]).          %% 217
-export([comp_lt/0]).          %% 218
-export([comp_geq/0]).         %% 219
-export([comp_leq/0]).         %% 220
-export([exact_eqeq/0]).       %% 223
-export([exact_neq/0]).        %% 224
-export([test_integer/0]).     %% 225
-export([test_float/0]).       %% 226
-export([test_number/0]).      %% 227
-export([test_atom/0]).        %% 228
-export([test_constant/0]).    %% 229
-export([test_list/0]).        %% 230
-export([test_tuple/0]).       %% 231
-export([test_pid/0]).         %% 232
-export([test_reference/0]).   %% 233
-export([test_port/0]).        %% 234
-export([stack_need/1]).       %% 235
-export([heap_need/1]).        %% 236
-export([comp_eqeq/0]).        %% 237
-export([comp_neq/0]).         %% 238
-export([debug_info/3]).       %% 239
-export([failFunction/0]).     %% 240
-export([test_binary/0]).      %% 241
-export([try_me_else/1]).      %% 242
-export([goto/1]).             %% 243
-export([call_local/2]).       %% 244
-export([enter_local/2]).      %% 245
-export([pushCatch/1]).        %% 246
-export([wait1/1]).            %% 247
-export([arith_neg/0]).        %% 250
-export([apply_enter/0]).      %% 251
-export([apply_call/0]).       %% 252
-export([bif_enter/2]).        %% 253
-export([bif_call/2]).         %% 254
-export([die/0]).              %% 255

-define(JAM_MAGIC_0, "Tue Jan 22 14:32:44 MET 1991").
-define(JAM_MAGIC_1, "1.0 Fri Feb 3 09:55:56 MET 1995").

-define(verbose(F, A), io:format((F),(A))).
%% -define(verbose(F, A), ok).

-define(MAX_OP_CODE, 255).
-type op() :: 1..?MAX_OP_CODE.

-type argtype() :: integer()|u8|u16|i24|i32|str16|bn|jatb16|p16|p24|p64.
-type u8() :: 0..16#ff.
-type u16() :: 0..16#ffff.
-type j24() :: -16#800000..16#7fffff.
-type type_bit() :: integer | float | atom | reference | port | pid |
		    tuple | nil | list | binary.

-record(opcode,
	{
	 mnemonic,     %% general mmnemoic storeVar, eq, arg ...
	 umnemonic,    %% unique mnemonic storeVar_0, eq_0, arg_1 ...
	 op :: op(),
	 arity :: integer(),
	 argtypes :: [argtype()]
	}).

-type instruction() :: {Mnemonic::atom(), Args::[term()]}.

%% bits for type op
-define(integer_bit,1).
-define(float_bit, 2).
-define(atom_bit,3).
-define(reference_bit, 4).
-define(port_bit,5).
-define(pid_bit,6).
-define(tuple_bit,7).
-define(nil_bit,8).
-define(list_bit,9).
-define(binary_bit,10).

-define(test_bit_map, 
	#{ 
	   ?integer_bit => integer,
	   ?float_bit => float,
	   ?atom_bit => atom,
	   ?reference_bit => reference,
	   ?port_bit => port,
	   ?pid_bit => pid,
	   ?tuple_bit => tuple,
	   ?nil_bit => nil,
	   ?list_bit => list,
	   ?binary_bit => binary
	 }).

-define(DEFINE_FUNCTION,  1).
-define(STRING, 2).
-define(EXPORTED,  3).
-define(LOCAL,  4).
-define(PATCH_CONST,  5).
-define(PATCH_LOCAL,  6).
-define(CSA,  7).
-define(PATCH_AT_ADDRESS,  14).
-define(CODE, 9).
-define(COPYRIGHT,  10).
-define(MAGIC,  11).
-define(PATCH_FLOAT,  12).
-define(FAT_CODE, 15).

-define(string(L,S), ?STRING,L:16,S:L/binary).
-define(code16(L,Code), ?CODE,L:16,Code:L/binary).
-define(code24(L,Code), ?CODE,L:24,Code:L/binary).

-define(is_u8(X), (((X) band (bnot 16#ff)) =:= 0)).
-define(is_u16(X), (((X) band (bnot 16#ffff)) =:= 0)).

-type import() :: {Mod::atom(),Fun::atom(),Arity::integer()}.
-type export() :: {Name::atom(),Arity::integer(),Entry::integer()}.
-type local()  :: {Name::atom(),Arity::integer(),Entry::integer()}.
-type line()   :: {Filename::string(),Lineno::integer()}.
-type label()  :: integer().

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
	 pos = 0,
	 atoms     :: #jctx_table{map::jctx_map(atom())},
	 literals  :: #jctx_table{map::jctx_map(term())},
	 imports   :: #jctx_table{map::jctx_map(import())},
	 exports   :: #jctx_table{map::jctx_map(export())},
	 locals    :: #jctx_table{map::jctx_map(local())},
	 lines     :: #jctx_table{map::jctx_map(line())},
	 labels    :: #jctx_table{map::jctx_map(label())},
	 strings   :: binary()
	}).


load_as_beam(Filename) ->
    case load_file(Filename) of
	{ok,JAM} ->
	    jamtobeam:function_list(JAM);
	Error -> Error
    end.

load_file(Filename) ->
    case file:read_file(Filename) of
	{ok,Bin}  ->
	    load_bin(Bin);
	Error -> Error
    end.

%% 
%% Load a JAM binary and return JAM assembler
%% 
-spec load_bin(JamBinary::binary()) ->
	  {ok,Jam::[instruction()]} | 
	  {error, Reason::term()}.
	  
load_bin(<<?MAGIC, ?string(L,Magic), Bin/binary>>) ->
    io:format("version: ~s\n", [Magic]),
    %% Version = {0,{version,Magic}}
    funcs(Bin,undefined,#{},[]);
load_bin(_) ->
    {error, bad_magic}.

funcs(<<?DEFINE_FUNCTION, ?string(L1,Mod), T, ?string(L2,Name), Arity,
	 ?code16(L3,Data), Bin/binary>>, Fun, Dict, Acc) ->
    Type = if T =:= ?EXPORTED -> exported;
	      T =:= ?LOCAL ->  local
	   end,
    Code = code(Data),
    F = {function,Type,{binary_to_atom(Mod),binary_to_atom(Name),Arity},Code},
    Acc1 = resolve(Fun,Dict,Acc),
    funcs(Bin,F,#{},Acc1);
funcs(<<?DEFINE_FUNCTION, ?string(L1,Mod), T, ?string(L2,Name), Arity,
	?code24(L3,Data), Bin/binary>>, Fun, Dict, Acc) ->
    Type = if T =:= ?EXPORTED -> exported;
	      T =:= ?LOCAL ->  local
	   end,
    Code = code(Data),
    F = {function,Type,{binary_to_atom(Mod),binary_to_atom(Name),Arity},Code},
    Acc1 = resolve(Fun,Dict,Acc),
    funcs(Bin,F,#{},Acc1);

funcs(<<?PATCH_CONST, ?string(L,Data), Bin/binary>>,Fun,Dict,Acc) ->
    Const = binary_to_atom(Data),
    {Dict1, Bin1} = patch_list(Bin,Const,Dict),
    funcs(Bin1,Fun,Dict1,Acc);
funcs(<<?PATCH_FLOAT, ?string(L,Data), Bin/binary>>,Fun,Dict,Acc) ->
    Float = binary_to_float(Data),
    {Dict1, Bin1} = patch_list(Bin,Float,Dict),
    funcs(Bin1,Fun,Dict1,Acc);
funcs(<<?PATCH_LOCAL, ?string(L,Data),Arity, Bin/binary>>,Fun,Dict,Acc) ->
    Func = binary_to_atom(Data),
    {Dict1, Bin1} = patch_list(Bin,{Func,Arity},Dict),
    funcs(Bin1,Fun,Dict1,Acc);
funcs(<<?CSA, ?string(Len1,Data1),?string(Len2,Data2),Arity,
	 Bin/binary>>,Fun,Dict,Acc) ->
    Mod = binary_to_atom(Data1),
    Func = binary_to_atom(Data2),
    {Dict1, Bin1} = patch_list(Bin,{Mod,Func,Arity},Dict),
    funcs(Bin1,Fun,Dict1,Acc);
funcs(<<>>,Fun,Dict,Acc) ->
    Acc1 = resolve(Fun,Dict,Acc),
    {ok, lists:reverse(Acc1)}.

patch_list(<<?PATCH_AT_ADDRESS,Addr:24,Bin1/binary>>,Term,Dict) ->
    ?verbose("patch: ~w => ~p\n", [Addr, Term]),
    patch_list(Bin1, Term, Dict#{ Addr => Term });
patch_list(Bin,_Term,Dict) ->
    {Dict, Bin}.

resolve(undefined,_Dict,Acc) ->
    Acc;
resolve({function,Type,MFA,Code},Dict,Acc) ->
    Code1 = lists:map(
	      fun({Pos,Instr}) when is_atom(Instr) -> 
		      {Pos,Instr};
		 ({Pos,Instr}) when is_tuple(Instr) ->
		      [Mnemoic|Args] = tuple_to_list(Instr),
		      Instr1 =
			  list_to_tuple(
			    [Mnemoic|
			     [resolve_arg(A,Pos,Dict) || A <- Args]]),
		      {Pos,Instr1}
	      end, Code),
    Targets = jump_targets(Code1, #{}),
    Code2 = mark_destinations(Code1, Targets),
    [{function,Type,MFA,Code2}|Acc].

resolve_arg({p,Addr}, _Pos, Dict) -> maps:get(Addr, Dict);
resolve_arg({j,Offs}, Pos, _Dict) -> {j,Pos+Offs+1};  %% abs address
resolve_arg(Arg, _Pos, _Dict) -> Arg.

%% Remove address and add labels where needed
mark_destinations([{Addr,I}|Is], Targets) ->
    case maps:find(Addr, Targets) of  %% is a target
	{ok, true} -> %% yes convert addr into a label
	    [{label,Addr},I | mark_destinations(Is, Targets)];
	error ->
	    [I|mark_destinations(Is, Targets)]
    end;
mark_destinations([],_Targets) ->
    [].

%% Find all labels/addresses reference from all instructions
jump_targets([{_Addr,I}|Is], Used) when is_atom(I) ->
    jump_targets(Is, Used);
jump_targets([{_Addr,{gotoix,_Low,Table}}|Is], Used) ->
    Used1 = lists:foldl(fun({j,Addr}, U) -> 
				U#{ Addr => true }
			end, Used, tuple_to_list(Table)),
    jump_targets(Is, Used1);		    
jump_targets([{_Addr,I}|Is], Used) when is_tuple(I) ->
    [_|Args] = tuple_to_list(I),
    Used1 = lists:foldl(fun({j,Addr}, U) -> U#{ Addr => true };
			   (_, U) -> U
			end, Used, Args),
    jump_targets(Is, Used1);
jump_targets([], Used) -> Used.


code(Bin) ->
    code(Bin, 1).
code(Bin, Pos) ->
    code(Bin, Pos, []).

code(<<>>, _Pos, Acc) -> 
    lists:reverse(Acc);
code(<<Opcode,Bin/binary>>,Pos,Acc) ->
    #opcode{mnemonic=Mnemonic,argtypes=ArgTypes} = 
	maps:get(Opcode, opcode_map()),
    ?verbose("decode opcode=~w, mnemonic=~p, args=~p\n",
	     [Opcode, Mnemonic, ArgTypes]),
    {Instr,Bin2} = case decode_args(ArgTypes,Bin,Pos) of
		       {[],Bin1} -> 
			   {Mnemonic, Bin1};
		       {Args,Bin1} ->
			   {list_to_tuple([Mnemonic|Args]),Bin1}
		   end,
    ?verbose("~w: ~w\n", [Pos, Instr]),
     Pos1 = Pos + ((byte_size(Bin)+1) - byte_size(Bin2)),
    code(Bin2, Pos1, [{Pos,Instr}|Acc]).


decode_args(Ts, Bin, Pos) ->
    decode_args_(Ts, Bin, Pos, []).
    
decode_args_([u8|Ts], <<U:8,Bin/binary>>,Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+1, [U|Acc]);
decode_args_([u16|Ts], <<U:16,Bin/binary>>,Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+2, [U|Acc]);
decode_args_([i24|Ts], <<I:24/signed,Bin/binary>>, Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+3, [I|Acc]);
decode_args_([j24|Ts], <<I:24/signed,Bin/binary>>, Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+3, [{j,I}|Acc]);
decode_args_([i32|Ts], <<I:32/signed,Bin/binary>>, Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+4, [I|Acc]);
%%decode_arglist_([f64|Ts], <<F:64/float,Bin/binary>>, Pos, Acc) ->
%%    decode_args_(Ts, Bin, Pos+8, [F|Acc]);
decode_args_([str16|Ts], <<Len:16,Str:Len/binary,Bin/binary>>, Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+2+Len, [binary_to_list(Str)|Acc]);
decode_args_([bn|Ts], <<Size:32,S,I:Size/little-unit:8,Bin/binary>>,Pos,
	       Acc) ->
    decode_args_(Ts, Bin, Pos+4+1+Size, [(1-S)*I|Acc]);
decode_args_([jtab16|Ts], <<N:16,Low:16,Tab:(N*3)/binary,Bin/binary>>,
	       Pos, Acc) ->
    JTab = [ {j, J} || <<J:24/signed>> <= Tab],
    decode_args_(Ts, Bin, Pos+2+2+N*3, 
		 [list_to_tuple(lists:reverse(JTab)),Low|Acc]);
%% patch positions
decode_args_([p16|Ts], <<_U:16,Bin/binary>>, Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+2, [{p,Pos}|Acc]);
decode_args_([p24|Ts], <<_U:24,Bin/binary>>, Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+3, [{p,Pos}|Acc]);
decode_args_([p64|Ts], <<_U:64,Bin/binary>>, Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+8, [{p,Pos}|Acc]);
decode_args_([{enum,u16,Map}|Ts], <<U:16,Bin/binary>>, Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+2, [decode_enum(U, Map)|Acc]);

decode_args_([{u8,U}|Ts], <<U:8,Bin/binary>>,Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+1, Acc);
decode_args_([{u16,U}|Ts], <<U:16,Bin/binary>>,Pos, Acc) ->
    decode_args_(Ts, Bin, Pos+2, Acc);

decode_args_([Val|Ts], Bin, Pos, Acc) when is_integer(Val) ->
    decode_args_(Ts, Bin, Pos, [Val|Acc]);
decode_args_([], Bin, _Pos, Acc) ->
    {lists:reverse(Acc), Bin}.

decode_enum(U, Map) ->
    decode_enum_(U, 0, Map, []).

decode_enum_(0, _I, _Map, Acc) -> Acc;
decode_enum_(U, I, Map, Acc) ->
    Bit = (1 bsl I),
    if U band Bit =:= 0 ->
	    decode_enum_(U, I+1, Map, Acc);
       true ->
	    U1 = U band (bnot Bit),
	    case maps:find(Bit, Map) of
		error ->
		    decode_enum_(U1, I+1, Map, [Bit|Acc]);
		{ok,Enum} ->
		    decode_enum_(U1, I+1, Map, [Enum|Acc])
	    end
    end.
    
-define(OP_pushVar_0, 1).
-define(OP_pushVar_1, 2).
-define(OP_pushVar_2, 3).
-define(OP_pushVar_3, 4).
-define(OP_pushVar_4, 5).
-define(OP_pushVar_5, 6).
-define(OP_pushVar_6, 7).
-define(OP_pushVar_7, 8).
-define(OP_pushVar_8, 9).
-define(OP_pushVar_9, 10).
-define(OP_pushVar_10, 11).
-define(OP_pushVar_11, 12).
-define(OP_pushVar_12, 13).
-define(OP_pushVar_13, 14).
-define(OP_pushVar_14, 15).
-define(OP_pushVar_15, 16).
-define(OP_pushVarN, 17).
-define(OP_storeVar_0, 18).
-define(OP_storeVar_1, 19).
-define(OP_storeVar_2, 20).
-define(OP_storeVar_3, 21).
-define(OP_storeVar_4, 22).
-define(OP_storeVar_5, 23).
-define(OP_storeVar_6, 24).
-define(OP_storeVar_7, 25).
-define(OP_storeVar_8, 26).
-define(OP_storeVar_9, 27).
-define(OP_storeVar_10, 28).
-define(OP_storeVar_11, 29).
-define(OP_storeVar_12, 30).
-define(OP_storeVar_13, 31).
-define(OP_storeVar_14, 32).
-define(OP_storeVar_15, 33).
-define(OP_storeVarN, 34).
-define(OP_alloc_0, 35).
-define(OP_alloc_1, 36).
-define(OP_alloc_2, 37).
-define(OP_alloc_3, 38).
-define(OP_alloc_4, 39).
-define(OP_alloc_5, 40).
-define(OP_alloc_6, 41).
-define(OP_alloc_7, 42).
-define(OP_alloc_8, 43).
-define(OP_alloc_9, 44).
-define(OP_alloc_10, 45).
-define(OP_alloc_11, 46).
-define(OP_alloc_12, 47).
-define(OP_alloc_13, 48).
-define(OP_alloc_14, 49).
-define(OP_alloc_15, 50).
-define(OP_allocN, 51).
-define(OP_eqVar_0, 52).
-define(OP_eqVar_1, 53).
-define(OP_eqVar_2, 54).
-define(OP_eqVar_3, 55).
-define(OP_eqVar_4, 56).
-define(OP_eqVar_5, 57).
-define(OP_eqVar_6, 58).
-define(OP_eqVar_7, 59).
-define(OP_eqVar_8, 60).
-define(OP_eqVar_9, 61).
-define(OP_eqVar_10, 62).
-define(OP_eqVar_11, 63).
-define(OP_eqVar_12, 64).
-define(OP_eqVar_13, 65).
-define(OP_eqVar_14, 66).
-define(OP_eqVar_15, 67).
-define(OP_eqVarN, 68).
-define(OP_eqArg_0, 69).
-define(OP_eqArg_1, 70).
-define(OP_eqArg_2, 71).
-define(OP_eqArg_3, 72).
-define(OP_eqArg_4, 73).
-define(OP_eqArg_5, 74).
-define(OP_eqArg_6, 75).
-define(OP_eqArg_7, 76).
-define(OP_eqArg_8, 77).
-define(OP_eqArg_9, 78).
-define(OP_eqArg_10, 79).
-define(OP_eqArg_11, 80).
-define(OP_eqArg_12, 81).
-define(OP_eqArg_13, 82).
-define(OP_eqArg_14, 83).
-define(OP_eqArg_15, 84).
-define(OP_eqArgN, 85).
-define(OP_arg_0, 86).
-define(OP_arg_1, 87).
-define(OP_arg_2, 88).
-define(OP_arg_3, 89).
-define(OP_arg_4, 90).
-define(OP_arg_5, 91).
-define(OP_arg_6, 92).
-define(OP_arg_7, 93).
-define(OP_arg_8, 94).
-define(OP_arg_9, 95).
-define(OP_arg_10, 96).
-define(OP_arg_11, 97).
-define(OP_arg_12, 98).
-define(OP_arg_13, 99).
-define(OP_arg_14, 100).
-define(OP_arg_15, 101).
-define(OP_argN, 102).
-define(OP_unpkTuple_0, 103).
-define(OP_unpkTuple_1, 104).
-define(OP_unpkTuple_2, 105).
-define(OP_unpkTuple_3, 106).
-define(OP_unpkTuple_4, 107).
-define(OP_unpkTuple_5, 108).
-define(OP_unpkTuple_6, 109).
-define(OP_unpkTuple_7, 110).
-define(OP_unpkTuple_8, 111).
-define(OP_unpkTuple_9, 112).
-define(OP_unpkTuple_10, 113).
-define(OP_unpkTuple_11, 114).
-define(OP_unpkTuple_12, 115).
-define(OP_unpkTuple_13, 116).
-define(OP_unpkTuple_14, 117).
-define(OP_unpkTuple_15, 118).
-define(OP_unpkTupleN, 119).
-define(OP_mkTuple_0, 120).
-define(OP_mkTuple_1, 121).
-define(OP_mkTuple_2, 122).
-define(OP_mkTuple_3, 123).
-define(OP_mkTuple_4, 124).
-define(OP_mkTuple_5, 125).
-define(OP_mkTuple_6, 126).
-define(OP_mkTuple_7, 127).
-define(OP_mkTuple_8, 128).
-define(OP_mkTuple_9, 129).
-define(OP_mkTuple_10, 130).
-define(OP_mkTuple_11, 131).
-define(OP_mkTuple_12, 132).
-define(OP_mkTuple_13, 133).
-define(OP_mkTuple_14, 134).
-define(OP_mkTuple_15, 135).
-define(OP_mkTupleN, 136).
-define(OP_getInt_0, 137).
-define(OP_getInt_1, 138).
-define(OP_getInt_2, 139).
-define(OP_getInt_3, 140).
-define(OP_getInt_4, 141).
-define(OP_getInt_5, 142).
-define(OP_getInt_6, 143).
-define(OP_getInt_7, 144).
-define(OP_getInt_8, 145).
-define(OP_getInt_9, 146).
-define(OP_getInt_10, 147).
-define(OP_getInt_11, 148).
-define(OP_getInt_12, 149).
-define(OP_getInt_13, 150).
-define(OP_getInt_14, 151).
-define(OP_getInt_15, 152).
-define(OP_getInt1, 153).
-define(OP_hash, 154).
-define(OP_getFloat, 155).
-define(OP_pushInt_0, 156).
-define(OP_pushInt_1, 157).
-define(OP_pushInt_2, 158).
-define(OP_pushInt_3, 159).
-define(OP_pushInt_4, 160).
-define(OP_pushInt_5, 161).
-define(OP_pushInt_6, 162).
-define(OP_pushInt_7, 163).
-define(OP_pushInt_8, 164).
-define(OP_pushInt_9, 165).
-define(OP_pushInt_10, 166).
-define(OP_pushInt_11, 167).
-define(OP_pushInt_12, 168).
-define(OP_pushInt_13, 169).
-define(OP_pushInt_14, 170).
-define(OP_pushInt_15, 171).
-define(OP_pushInt1, 172).
-define(OP_type, 173).
-define(OP_pushFloat, 174).
-define(OP_nodebug_info, 175).
-define(OP_list_length, 176).
-define(OP_getAtom, 177).
-define(OP_try_me_else_fail, 178).
-define(OP_commit, 179).
-define(OP_ret, 180).
-define(OP_getNil, 181).
-define(OP_pushNil, 182).
-define(OP_pop, 183).
-define(OP_mkList, 184).
-define(OP_unpkList, 185).
-define(OP_send, 186).
-define(OP_self, 187).
-define(OP_pushStr, 188).
-define(OP_pushAtom, 189).
-define(OP_getStr, 190).
-define(OP_call_remote, 191).
-define(OP_head, 192).
-define(OP_enter_remote, 193).
-define(OP_dup, 194).
-define(OP_popCommit, 195).
-define(OP_failIf, 196).
-define(OP_failCase, 197).
-define(OP_tail, 198).
-define(OP_popCatch, 199).
-define(OP_setTimeout, 200).
-define(OP_wait, 201).
-define(OP_gotoix, 202).
-define(OP_popCommitJoin, 203).
-define(OP_save, 204).
-define(OP_arith_plus, 205).
-define(OP_arith_minus, 206).
-define(OP_arith_times, 207).
-define(OP_arith_div, 208).
-define(OP_arith_intdiv, 209).
-define(OP_arith_band, 210).
-define(OP_arith_bor, 211).
-define(OP_arith_bxor, 212).
-define(OP_arith_bnot, 213).
-define(OP_arith_bsl, 214).
-define(OP_arith_bsr, 215).
-define(OP_arith_rem, 216).
-define(OP_comp_gt, 217).
-define(OP_comp_lt, 218).
-define(OP_comp_geq, 219).
-define(OP_comp_leq, 220).
-define(OP_pushIntN, 221).
-define(OP_getIntN, 222).
-define(OP_exact_eqeq, 223).
-define(OP_exact_neq, 224).
-define(OP_test_integer, 225).
-define(OP_test_float, 226).
-define(OP_test_number, 227).
-define(OP_test_atom, 228).
-define(OP_test_constant, 229).
-define(OP_test_list, 230).
-define(OP_test_tuple, 231).
-define(OP_test_pid, 232).
-define(OP_test_reference, 233).
-define(OP_test_port, 234).
-define(OP_stack_need, 235).
-define(OP_heap_need, 236).
-define(OP_comp_eqeq, 237).
-define(OP_comp_neq, 238).
-define(OP_debug_info, 239).
-define(OP_failFunction, 240).
-define(OP_test_binary, 241).
-define(OP_try_me_else, 242).
-define(OP_goto, 243).
-define(OP_call_local, 244).
-define(OP_enter_local, 245).
-define(OP_pushCatch, 246).
-define(OP_wait1, 247).
-define(OP_pushInt4, 248).
-define(OP_getInt4, 249).
-define(OP_arith_neg, 250).
-define(OP_apply_enter, 251).
-define(OP_apply_call, 252).
-define(OP_bif_enter, 253).
-define(OP_bif_call, 254).
-define(OP_die, 255).

-define(OPENT(Op,Mnemonic,As),
	(Op) => #opcode{mnemonic=(Mnemonic),
			umnemonic=(Mnemonic),
			op=(Op),
			arity = length(As),
			argtypes = (As)
		       },
	(Mnemonic) => (Op)).

-define(OPENT(Op,UMnemonic,Mnemonic,As),
	(Op) => #opcode{mnemonic=(Mnemonic),
			umnemonic=(UMnemonic),
			op=(Op),
			arity = length(As),
			argtypes = (As)
		       },
	(UMnemonic) => (Op)).



-spec opcode_map() -> #{ integer() => #opcode{},
			 atom() => op() }.

opcode_map() ->
#{
?OPENT(?OP_pushVar_0,  pushVar_0,  pushVar, [0]),
?OPENT(?OP_pushVar_1,  pushVar_1,  pushVar, [1]),
?OPENT(?OP_pushVar_2,  pushVar_2,  pushVar, [2]),
?OPENT(?OP_pushVar_3,  pushVar_3,  pushVar, [3]),
?OPENT(?OP_pushVar_4,  pushVar_4,  pushVar, [4]),
?OPENT(?OP_pushVar_5,  pushVar_5,  pushVar, [5]),
?OPENT(?OP_pushVar_6,  pushVar_6,  pushVar, [6]),
?OPENT(?OP_pushVar_7,  pushVar_7,  pushVar, [7]),
?OPENT(?OP_pushVar_8,  pushVar_8,  pushVar, [8]),
?OPENT(?OP_pushVar_9,  pushVar_9,  pushVar, [9]),
?OPENT(?OP_pushVar_10,  pushVar_10, pushVar, [10]),
?OPENT(?OP_pushVar_11,  pushVar_11, pushVar, [11]),
?OPENT(?OP_pushVar_12,  pushVar_12, pushVar, [12]),
?OPENT(?OP_pushVar_13,  pushVar_13, pushVar, [13]),
?OPENT(?OP_pushVar_14,  pushVar_14, pushVar, [14]),
?OPENT(?OP_pushVar_15,  pushVar_15, pushVar, [15]),
?OPENT(?OP_pushVarN,   pushVar, [u8]),
?OPENT(?OP_storeVar_0,  storeVar_0, storeVar, [0]),
?OPENT(?OP_storeVar_1,  storeVar_1, storeVar, [1]),
?OPENT(?OP_storeVar_2,  storeVar_2, storeVar, [2]),
?OPENT(?OP_storeVar_3,  storeVar_3, storeVar, [3]),
?OPENT(?OP_storeVar_4,  storeVar_4, storeVar, [4]),
?OPENT(?OP_storeVar_5,  storeVar_5, storeVar, [5]),
?OPENT(?OP_storeVar_6,  storeVar_6, storeVar, [6]),
?OPENT(?OP_storeVar_7,  storeVar_7, storeVar, [7]),
?OPENT(?OP_storeVar_8,  storeVar_8, storeVar, [8]),
?OPENT(?OP_storeVar_9,  storeVar_9, storeVar, [9]),
?OPENT(?OP_storeVar_10,  storeVar_10, storeVar, [10]),
?OPENT(?OP_storeVar_11,  storeVar_11, storeVar, [11]),
?OPENT(?OP_storeVar_12,  storeVar_12, storeVar, [12]),
?OPENT(?OP_storeVar_13,  storeVar_13, storeVar, [13]),
?OPENT(?OP_storeVar_14,  storeVar_14, storeVar, [14]),
?OPENT(?OP_storeVar_15,  storeVar_15, storeVar, [15]),
?OPENT(?OP_storeVarN,   storeVar, [u8]),
?OPENT(?OP_alloc_0,  alloc_0,  alloc, [0]),
?OPENT(?OP_alloc_1,  alloc_1,  alloc, [1]),
?OPENT(?OP_alloc_2,  alloc_2,  alloc, [2]),
?OPENT(?OP_alloc_3,  alloc_3,  alloc, [3]),
?OPENT(?OP_alloc_4,  alloc_4,  alloc, [4]),
?OPENT(?OP_alloc_5,  alloc_5,  alloc, [5]),
?OPENT(?OP_alloc_6,  alloc_6,  alloc, [6]),
?OPENT(?OP_alloc_7,  alloc_7,  alloc, [7]),
?OPENT(?OP_alloc_8,  alloc_8,  alloc, [8]),
?OPENT(?OP_alloc_9,  alloc_9,  alloc, [9]),
?OPENT(?OP_alloc_10,  alloc_10, alloc, [10]),
?OPENT(?OP_alloc_11,  alloc_11, alloc, [11]),
?OPENT(?OP_alloc_12,  alloc_12, alloc, [12]),
?OPENT(?OP_alloc_13,  alloc_13, alloc, [13]),
?OPENT(?OP_alloc_14,  alloc_14, alloc, [14]),
?OPENT(?OP_alloc_15,  alloc_15, alloc, [15]),
?OPENT(?OP_allocN,   alloc, [u8]),
?OPENT(?OP_eqVar_0,  eqVar_0,  eqVar, [0]),
?OPENT(?OP_eqVar_1,  eqVar_1,  eqVar, [1]),
?OPENT(?OP_eqVar_2,  eqVar_2,  eqVar, [2]),
?OPENT(?OP_eqVar_3,  eqVar_3,  eqVar, [3]),
?OPENT(?OP_eqVar_4,  eqVar_4,  eqVar, [4]),
?OPENT(?OP_eqVar_5,  eqVar_5,  eqVar, [5]),
?OPENT(?OP_eqVar_6,  eqVar_6,  eqVar, [6]),
?OPENT(?OP_eqVar_7,  eqVar_7,  eqVar, [7]),
?OPENT(?OP_eqVar_8,  eqVar_8,  eqVar, [8]),
?OPENT(?OP_eqVar_9,  eqVar_9,  eqVar, [9]),
?OPENT(?OP_eqVar_10,  eqVar_10, eqVar, [10]),
?OPENT(?OP_eqVar_11,  eqVar_11, eqVar, [11]),
?OPENT(?OP_eqVar_12,  eqVar_12, eqVar, [12]),
?OPENT(?OP_eqVar_13,  eqVar_13, eqVar, [13]),
?OPENT(?OP_eqVar_14,  eqVar_14, eqVar, [14]),
?OPENT(?OP_eqVar_15,  eqVar_15, eqVar, [15]),
?OPENT(?OP_eqVarN,   eqVar, [u8]),
?OPENT(?OP_eqArg_0,  eqArg_0,  eqArg, [0]),
?OPENT(?OP_eqArg_1,  eqArg_1,  eqArg, [1]),
?OPENT(?OP_eqArg_2,  eqArg_2,  eqArg, [2]),
?OPENT(?OP_eqArg_3,  eqArg_3,  eqArg, [3]),
?OPENT(?OP_eqArg_4,  eqArg_4,  eqArg, [4]),
?OPENT(?OP_eqArg_5,  eqArg_5,  eqArg, [5]),
?OPENT(?OP_eqArg_6,  eqArg_6,  eqArg, [6]),
?OPENT(?OP_eqArg_7,  eqArg_7,  eqArg, [7]),
?OPENT(?OP_eqArg_8,  eqArg_8,  eqArg, [8]),
?OPENT(?OP_eqArg_9,  eqArg_9,  eqArg, [9]),
?OPENT(?OP_eqArg_10,  eqArg_10, eqArg, [10]),
?OPENT(?OP_eqArg_11,  eqArg_11, eqArg, [11]),
?OPENT(?OP_eqArg_12,  eqArg_12, eqArg, [12]),
?OPENT(?OP_eqArg_13,  eqArg_13, eqArg, [13]),
?OPENT(?OP_eqArg_14,  eqArg_14, eqArg, [14]),
?OPENT(?OP_eqArg_15,  eqArg_15, eqArg, [15]),
?OPENT(?OP_eqArgN,   eqArg, [u8]),
?OPENT(?OP_arg_0,  arg_0,  arg, [0]),
?OPENT(?OP_arg_1,  arg_1,  arg, [1]),
?OPENT(?OP_arg_2,  arg_2,  arg, [2]),
?OPENT(?OP_arg_3,  arg_3,  arg, [3]),
?OPENT(?OP_arg_4,  arg_4,  arg, [4]),
?OPENT(?OP_arg_5,  arg_5,  arg, [5]),
?OPENT(?OP_arg_6,  arg_6,  arg, [6]),
?OPENT(?OP_arg_7,  arg_7,  arg, [7]),
?OPENT(?OP_arg_8,  arg_8,  arg, [8]),
?OPENT(?OP_arg_9,  arg_9,  arg, [9]),
?OPENT(?OP_arg_10,  arg_10, arg, [10]),
?OPENT(?OP_arg_11,  arg_11, arg, [11]),
?OPENT(?OP_arg_12,  arg_12, arg, [12]),
?OPENT(?OP_arg_13,  arg_13, arg, [13]),
?OPENT(?OP_arg_14,  arg_14, arg, [14]),
?OPENT(?OP_arg_15,  arg_15, arg, [15]),
?OPENT(?OP_argN,   arg, [u8]),
?OPENT(?OP_unpkTuple_0,  unpkTuple_0, unpkTuple, [0]),
?OPENT(?OP_unpkTuple_1,  unpkTuple_1, unpkTuple, [1]),
?OPENT(?OP_unpkTuple_2,  unpkTuple_2, unpkTuple, [2]),
?OPENT(?OP_unpkTuple_3,  unpkTuple_3, unpkTuple, [3]),
?OPENT(?OP_unpkTuple_4,  unpkTuple_4, unpkTuple, [4]),
?OPENT(?OP_unpkTuple_5,  unpkTuple_5, unpkTuple, [5]),
?OPENT(?OP_unpkTuple_6,  unpkTuple_6, unpkTuple, [6]),
?OPENT(?OP_unpkTuple_7,  unpkTuple_7, unpkTuple, [7]),
?OPENT(?OP_unpkTuple_8,  unpkTuple_8, unpkTuple, [8]),
?OPENT(?OP_unpkTuple_9,  unpkTuple_9, unpkTuple, [9]),
?OPENT(?OP_unpkTuple_10,  unpkTuple_10, unpkTuple, [10]),
?OPENT(?OP_unpkTuple_11,  unpkTuple_11, unpkTuple, [11]),
?OPENT(?OP_unpkTuple_12,  unpkTuple_12, unpkTuple, [12]),
?OPENT(?OP_unpkTuple_13,  unpkTuple_13, unpkTuple, [13]),
?OPENT(?OP_unpkTuple_14,  unpkTuple_14, unpkTuple, [14]),
?OPENT(?OP_unpkTuple_15,  unpkTuple_15, unpkTuple, [15]),
?OPENT(?OP_unpkTupleN, unpkTuple, [u8]),
?OPENT(?OP_mkTuple_0,  mkTuple_0, mkTuple, [0]),
?OPENT(?OP_mkTuple_1,  mkTuple_1, mkTuple, [1]),
?OPENT(?OP_mkTuple_2,  mkTuple_2, mkTuple, [2]),
?OPENT(?OP_mkTuple_3,  mkTuple_3, mkTuple, [3]),
?OPENT(?OP_mkTuple_4,  mkTuple_4, mkTuple, [4]),
?OPENT(?OP_mkTuple_5,  mkTuple_5, mkTuple, [5]),
?OPENT(?OP_mkTuple_6,  mkTuple_6, mkTuple, [6]),
?OPENT(?OP_mkTuple_7,  mkTuple_7, mkTuple, [7]),
?OPENT(?OP_mkTuple_8,  mkTuple_8, mkTuple, [8]),
?OPENT(?OP_mkTuple_9,  mkTuple_9, mkTuple, [9]),
?OPENT(?OP_mkTuple_10,  mkTuple_10, mkTuple, [10]),
?OPENT(?OP_mkTuple_11,  mkTuple_11, mkTuple, [11]),
?OPENT(?OP_mkTuple_12,  mkTuple_12, mkTuple, [12]),
?OPENT(?OP_mkTuple_13,  mkTuple_13, mkTuple, [13]),
?OPENT(?OP_mkTuple_14,  mkTuple_14, mkTuple, [14]),
?OPENT(?OP_mkTuple_15,  mkTuple_15, mkTuple, [15]),
?OPENT(?OP_mkTupleN, mkTuple, [u8]),
?OPENT(?OP_getInt_0,  getInt_0, getInt, [0]),
?OPENT(?OP_getInt_1,  getInt_1, getInt, [1]),
?OPENT(?OP_getInt_2,  getInt_2, getInt, [2]),
?OPENT(?OP_getInt_3,  getInt_3, getInt, [3]),
?OPENT(?OP_getInt_4,  getInt_4, getInt, [4]),
?OPENT(?OP_getInt_5,  getInt_5, getInt, [5]),
?OPENT(?OP_getInt_6,  getInt_6, getInt, [6]),
?OPENT(?OP_getInt_7,  getInt_7, getInt, [7]),
?OPENT(?OP_getInt_8,  getInt_8, getInt, [8]),
?OPENT(?OP_getInt_9,  getInt_9, getInt, [9]),
?OPENT(?OP_getInt_10,  getInt_10, getInt, [10]),
?OPENT(?OP_getInt_11,  getInt_11, getInt, [11]),
?OPENT(?OP_getInt_12,  getInt_12, getInt, [12]),
?OPENT(?OP_getInt_13,  getInt_13, getInt, [13]),
?OPENT(?OP_getInt_14,  getInt_14, getInt, [14]),
?OPENT(?OP_getInt_15,  getInt_15, getInt, [15]),
?OPENT(?OP_getInt1, getInt, [u8]),
?OPENT(?OP_hash, hash, [u16]),
?OPENT(?OP_getFloat, getFloat, [p64]),
?OPENT(?OP_pushInt_0,  pushInt_0, pushInt, [0]),
?OPENT(?OP_pushInt_1,  pushInt_1, pushInt, [1]),
?OPENT(?OP_pushInt_2,  pushInt_2, pushInt, [2]),
?OPENT(?OP_pushInt_3,  pushInt_3, pushInt, [3]),
?OPENT(?OP_pushInt_4,  pushInt_4, pushInt, [4]),
?OPENT(?OP_pushInt_5,  pushInt_5, pushInt, [5]),
?OPENT(?OP_pushInt_6,  pushInt_6, pushInt, [6]),
?OPENT(?OP_pushInt_7,  pushInt_7, pushInt, [7]),
?OPENT(?OP_pushInt_8,  pushInt_8, pushInt, [8]),
?OPENT(?OP_pushInt_9,  pushInt_9, pushInt, [9]),
?OPENT(?OP_pushInt_10,  pushInt_10, pushInt, [10]),
?OPENT(?OP_pushInt_11,  pushInt_11, pushInt, [11]),
?OPENT(?OP_pushInt_12,  pushInt_12, pushInt, [12]),
?OPENT(?OP_pushInt_13,  pushInt_13, pushInt, [13]),
?OPENT(?OP_pushInt_14,  pushInt_14, pushInt, [14]),
?OPENT(?OP_pushInt_15,  pushInt_15, pushInt, [15]),
?OPENT(?OP_pushInt1, pushInt, [u8]),
?OPENT(?OP_type, type, [{enum,u16,?test_bit_map}]),
?OPENT(?OP_pushFloat, pushFloat, [p64]),
?OPENT(?OP_nodebug_info, nodebug_info, [u8,p16,p16]),
?OPENT(?OP_list_length, list_length, []),
?OPENT(?OP_getAtom, getAtom, [p16]),
?OPENT(?OP_try_me_else_fail, try_me_else_fail, []),
?OPENT(?OP_commit, commit, []),
?OPENT(?OP_ret, ret, []),
?OPENT(?OP_getNil, getNil, []),
?OPENT(?OP_pushNil, pushNil, []),
?OPENT(?OP_pop, pop, []),
?OPENT(?OP_mkList, mkList, []),
?OPENT(?OP_unpkList, unpkList, []),
?OPENT(?OP_send, send, []),
?OPENT(?OP_self, self, []),
?OPENT(?OP_pushStr, pushStr, [str16]),
?OPENT(?OP_pushAtom, pushAtom, [p16]),
?OPENT(?OP_getStr, getStr, [str16]),
?OPENT(?OP_call_remote, call_remote, [u8,p16]),
?OPENT(?OP_head, head, []),
?OPENT(?OP_enter_remote, enter_remote, [u8,p16]),
?OPENT(?OP_dup, dup, []),
?OPENT(?OP_popCommit, popCommit, []),
?OPENT(?OP_failIf, failIf, []),
?OPENT(?OP_failCase, failCase, []),
?OPENT(?OP_tail, tail, []),
?OPENT(?OP_popCatch, popCatch, []),
?OPENT(?OP_setTimeout, setTimeout, []),
?OPENT(?OP_wait, wait, []),
?OPENT(?OP_gotoix, gotoix, [jtab16]),
?OPENT(?OP_popCommitJoin, popCommitJoin, []),
?OPENT(?OP_save, save, []),
?OPENT(?OP_arith_plus, arith_plus, []),
?OPENT(?OP_arith_minus, arith_minus, []),
?OPENT(?OP_arith_times, arith_times, []),
?OPENT(?OP_arith_div, arith_div, []),
?OPENT(?OP_arith_intdiv, arith_intdiv, []),
?OPENT(?OP_arith_band, arith_band, []),
?OPENT(?OP_arith_bor, arith_bor, []),
?OPENT(?OP_arith_bxor, arith_bxor, []),
?OPENT(?OP_arith_bnot, arith_bnot, []),
?OPENT(?OP_arith_bsl, arith_bsl, []),
?OPENT(?OP_arith_bsr, arith_bsr, []),
?OPENT(?OP_arith_rem, arith_rem, []),
?OPENT(?OP_comp_gt, comp_gt, []),
?OPENT(?OP_comp_lt, comp_lt, []),
?OPENT(?OP_comp_geq, comp_geq, []),
?OPENT(?OP_comp_leq, comp_leq, []),
?OPENT(?OP_pushIntN, pushInt_n, pushInt, [bn]),
?OPENT(?OP_getIntN, getInt_n, getInt, [bn]),
?OPENT(?OP_exact_eqeq, exact_eqeq, []),
?OPENT(?OP_exact_neq, exact_neq, []),
?OPENT(?OP_test_integer, test_integer, []),
?OPENT(?OP_test_float, test_float, []),
?OPENT(?OP_test_number, test_number, []),
?OPENT(?OP_test_atom, test_atom, []),
?OPENT(?OP_test_constant, test_constant, []),
?OPENT(?OP_test_list, test_list, []),
?OPENT(?OP_test_tuple, test_tuple, []),
?OPENT(?OP_test_pid, test_pid, []),
?OPENT(?OP_test_reference, test_reference, []),
?OPENT(?OP_test_port, test_port, []),
?OPENT(?OP_stack_need, stack_need, [u16]),
?OPENT(?OP_heap_need, heap_need, [u16]),
?OPENT(?OP_comp_eqeq, comp_eqeq, []),
?OPENT(?OP_comp_neq, comp_neq, []),
?OPENT(?OP_debug_info, debug_info, [u8,p16,p16,{u8,0}]),
?OPENT(?OP_failFunction, failFunction, []),
?OPENT(?OP_test_binary, test_binary, []),
?OPENT(?OP_try_me_else, try_me_else, [j24]),
?OPENT(?OP_goto, goto, [j24]),
?OPENT(?OP_call_local, call_local, [u8,p24]),
?OPENT(?OP_enter_local, enter_local, [u8,p24]),
?OPENT(?OP_pushCatch, pushCatch, [j24]),
?OPENT(?OP_wait1, wait1, [j24]),
?OPENT(?OP_pushInt4, pushInt_i32, pushInt, [i32]),
?OPENT(?OP_getInt4, getInt_i32, getInt, [i32]),
?OPENT(?OP_arith_neg, arith_neg, []),
?OPENT(?OP_apply_enter, apply_enter, [{u8,0},{u16,0}]),
?OPENT(?OP_apply_call, apply_call, [{u8,0},{u16,0}]),
?OPENT(?OP_bif_enter, bif_enter, [u8,u16]),
?OPENT(?OP_bif_call, bif_call, [u8,u16]),
?OPENT(?OP_die, die, [])
}.

%% @doc push variable V from stack frame onto the stack
%%      <h4>opcode: 1..16,17</h4>
%% @end
-spec pushVar(V::u8()) -> ok.

pushVar(V) when ?is_u8(V) ->
    emit_instruction(pushVar, [V]).

%% @doc store value from tos into variable V on stack frame
%%      <h4>opcode: 18..33,34</h4>
%% @end
-spec storeVar(V::u8()) -> ok.
storeVar(V) when ?is_u8(V) ->
    emit_instruction(storeVar, [V]).

%% @doc Allocate Size elements for variables in current stack frame
%%      <h4>opcode: 35..50,51</h4>
%% @end
-spec alloc(Size::u8()) -> ok.
alloc(Size) when ?is_u8(Size) ->
    emit_instruction(alloc, [Size]).

%% @doc test if TOS is equal to content at stack frame position V
%%      <h4>opcode: 52..67,68 </h4>
%% @end
-spec eqVar(V::u8()) -> ok.
eqVar(V) when ?is_u8(V) ->
    emit_instruction(eqVar, [V]).

%% @doc test if TOS is equal to argument number I
%%      <h4>opcode: 69..84,85</h4>
%% @end
-spec eqArg(I::u8()) -> ok.
eqArg(I) when ?is_u8(I) ->
    emit_instruction(eqArg, [I]).

%% @doc push argument I onto the stack
%%      <h4>opcode: 86..101,102</h4>
%% @end
-spec arg(I::u8()) -> ok.
arg(I) when ?is_u8(I) ->
    emit_instruction(arg, [I]).

%% @doc push N elements from a N-tuple onto the stack,
%%      fail if value on tos is not a tuple or if the 
%%      tuple is not N elements. The elements are push 
%%      from tuple index N to 1 so that the FIRST element
%%      in the tuple is on top of stack
%%      <h4>opcode: 103..118,119</h4>
%% @end
-spec unpkTuple(N::u8()) -> ok.
unpkTuple(N) when ?is_u8(N) ->
    emit_instruction(unpkTuple, [N]).

%% @doc Create a N tuple from the N top most elements on stack
%%      N elements are also popped of the stack
%%      The top of stack is the LAST element in the tuple
%%      <h4>opcode: 120..135,136</h4>
%% @end
-spec mkTuple(N::u8()) -> ok.
mkTuple(N) when ?is_u8(N) ->
    emit_instruction(mkTuple, [N]).

%% @doc test if TOS is equal to a constant integer
%%      <h4>opcode: 137..152,153,222</h4>
%% @end

getInt(Value) when is_integer(Value) ->
    emit_instruction(getInt, [Value]).

%% @doc Compute jam specific hash function for TOS
%%      replacing TOS with the hash value module 16-bit argument
%%      <h4>opcode: 154</h4>
%% @end
-spec hash(K::u16()) -> ok.
hash(K) when ?is_u16(K) -> 
    emit_instruction(hash, [K]).

%% @doc test if TOS is equal to a constant Floating point argument
%%      <h4>opcode: 155</h4>
%% @end
-spec getFloat(F::float()) -> ok.

getFloat(F) when is_float(F) ->
    emit_instruction(getFloat, [F]).

%% @doc Push a constant integer onto stack
%%      <h4>opcode: 156..171,172,221</h4>
%% @end
-spec pushInt(Value::integer()) -> ok.
pushInt(Value) when is_integer(Value) ->
    emit_instruction(pushInt, [Value]).

%% @doc test if TOS is of any type in type mask
%%      <h4>opcode: 173</h4>
%% @end
-spec type(Mask::[type_bit()]) -> ok.
type(Mask) when is_list(Mask) ->
    emit_instruction(type, [Mask]).

%% @doc Push a constant floating point value onto stack
%%      <h4>opcode: 174</h4>
%% @end
-spec pushFloat(F::float()) -> ok.
pushFloat(F) when is_float(F) ->
    emit_instruction(pushFloat, []).

%% @doc %comment-here%%
%%      <h4>opcode: 175</h4>
%% @end
-spec nodebug_info(M::atom(),F::atom(),A::u8()) -> ok.
nodebug_info(M,F,A) when is_atom(M), is_atom(F), ?is_u8(A) ->
    emit_instruction(nodebug_info, [A,M,F]).

%% @doc Calculate length of the list on top of stack and
%% 
%%      <h4>opcode: 176</h4>
%% @end
-spec list_length() -> ok.
list_length() ->
    emit_instruction(length, []).

%% @doc test if TOS is equal to constant atom A
%%      <h4>opcode: 177</h4>
%% @end
-spec getAtom(A::atom()) -> ok.
getAtom(A) when is_atom(A)->
    emit_instruction(getAtom, [A]).

%% @doc %comment-here%%
%%      <h4>opcode: 178</h4>
%% @end
-spec try_me_else_fail() -> ok.
try_me_else_fail() ->
    emit_instruction(fail, []).

%% @doc %comment-here%%
%%      <h4>opcode: 179</h4>
%% @end
-spec commit() -> ok.
commit() ->
    emit_instruction(commit, []).

%% @doc return TOS to calling function 
%%      <h4>opcode: 180</h4>
%% @end
-spec ret() -> ok.
ret() ->
    emit_instruction(ret, []).

%% @doc test if TOS is the empty list
%%      <h4>opcode: 181</h4>
%% @end

getNil() ->
    emit_instruction(getNil, []).

%% @doc push empty list (nil) onto stack
%%      <h4>opcode: 182</h4>
%% @end

pushNil() ->
    emit_instruction(pushNil, []).

%% @doc Pop stack (drop top element)
%%      <h4>opcode: 183</h4>
%% @end

pop() ->
    emit_instruction(pop, []).

%% @doc %comment-here%%
%%      <h4>opcode: 184</h4>
%% @end

mkList() ->
    emit_instruction(mkList, []).

%% @doc %comment-here%%
%%      <h4>opcode: 185</h4>
%% @end

unpkList() ->
    emit_instruction(unpkList, []).

%% @doc %comment-here%%
%%      <h4>opcode: 186</h4>
%% @end

send() ->
    emit_instruction(send, []).

%% @doc Push process id of current process onto stack
%%      <h4>opcode: 187</h4>
%% @end
-spec self() -> ok.
self() ->
    emit_instruction(self, []).

%% @doc %comment-here%%
%%      <h4>opcode: 188</h4>
%% @end
-spec pushStr(Str::string()) -> ok.
pushStr(Str) when is_list(Str) ->
    emit_instruction(pushStr, [Str]).

%% @doc push the atom A onto stack
%%      <h4>opcode: 189</h4>
%% @end
-spec pushAtom(A::atom()) -> ok.
pushAtom(A) when is_atom(A) ->
    emit_instruction(pushAtom, [A]).

%% @doc test if TOS is equal to constant string
%%      <h4>opcode: 190</h4>
%% @end
-spec getStr(Str::string()) -> ok.
getStr(Str) when is_list(Str) ->
    emit_instruction(getStr, [Str]).

%% @doc %comment-here%%
%%      <h4>opcode: 191</h4>
%% @end
-spec call_remote(M::atom(),F::atom(),A::u8()) -> ok.
call_remote(M,F,A) when is_atom(M), is_atom(F), ?is_u8(A) ->
    emit_instruction(call_remote, [A,{M,F,A}]).

%% @doc Push head/car of a list found on top of stack
%%      <h4>opcode: 192</h4>
%% @end
-spec head() -> ok.
head() ->
    emit_instruction(head, []).

%% @doc %comment-here%%
%%      <h4>opcode: 193</h4>
%% @end
-spec enter_remote(M::atom(),F::atom(),A::u8()) -> ok.
enter_remote(M,F,A) when is_atom(M), is_atom(F), ?is_u8(A) ->
    emit_instruction(enter_remote, [A,{M,F,A}]).

%% @doc Duplicate top of stack
%%      <h4>opcode: 194</h4>
%% @end
-spec dup() -> ok.
dup() ->
    emit_instruction(dup, []).

%% @doc %comment-here%%
%%      <h4>opcode: 195</h4>
%% @end

popCommit() ->
    emit_instruction(popCommit, []).

%% @doc %comment-here%%
%%      <h4>opcode: 196</h4>
%% @end
-spec failIf() -> ok.
failIf() ->
    emit_instruction(failIf, []).

%% @doc %comment-here%%
%%      <h4>opcode: 197</h4>
%% @end
-spec failCase() -> ok.
failCase() ->
    emit_instruction(failCase, []).

%% @doc Push tail/cdr of a list found on top of stack
%%      <h4>opcode: 198</h4>
%% @end
-spec tail() -> ok.
tail() ->
    emit_instruction(tail, []).

%% @doc %comment-here%%
%%      <h4>opcode: 199</h4>
%% @end
-spec popCatch() -> ok.
popCatch() ->
    emit_instruction(popCatch, []).

%% @doc %comment-here%%
%%      <h4>opcode: 200</h4>
%% @end
-spec setTimeout() -> ok.
setTimeout() ->
    emit_instruction(setTimeout, []).

%% @doc %comment-here%%
%%      <h4>opcode: 201</h4>
%% @end
-spec wait() -> ok.
wait() ->
    emit_instruction(wait, []).

%% @doc %comment-here%%
%%      <h4>opcode: 202</h4>
%% @end
-spec gotoix(Low::u16(), Table::[j24()]) -> ok.
gotoix(Low, Table) when ?is_u16(Low), is_list(Table) ->
    emit_instruction(gotoix, [Low, Table]).

%% @doc %comment-here%%
%%      <h4>opcode: 203</h4>
%% @end
-spec popCommitJoin() -> ok.
popCommitJoin() ->
    emit_instruction(popCommitJoin, []).

%% @doc %comment-here%%
%%      <h4>opcode: 204</h4>
%% @end
-spec save() -> ok.
save() ->
    emit_instruction(save, []).

%% @doc calculate A=pop(), B=pop(), push(B + A)
%%      <h4>opcode: 205</h4>
%% @end
-spec arith_plus() -> ok.
arith_plus() ->
    emit_instruction(arith_plus, []).

%% @doc calculate A=pop(), B=pop(), push(B - A)
%%      <h4>opcode: 206</h4>
%% @end
-spec arith_minus() -> ok.
arith_minus() ->
    emit_instruction(arith_minus, []).

%% @doc calculate A=pop(), B=pop(), push(B * A)
%%      <h4>opcode: 207</h4>
%% @end
-spec arith_times() -> ok.
arith_times() ->
    emit_instruction(arith_times, []).

%% @doc calculate A=pop(), B=pop(), push(B / A)
%%      <h4>opcode: 208</h4>
%% @end
-spec arith_div() -> ok.
arith_div() ->
    emit_instruction(arith_div, []).

%% @doc calculate A=pop(), B=pop(), push(B div A)
%%      <h4>opcode: 209</h4>
%% @end
-spec arith_intdiv() -> ok.
arith_intdiv() ->
    emit_instruction(arith_intdiv, []).

%% @doc %comment-here%%
%%      <h4>opcode: 210</h4>
%% @end
-spec arith_band() -> ok.
arith_band() ->
    emit_instruction(arith_band, []).

%% @doc %comment-here%%
%%      <h4>opcode: 211</h4>
%% @end
-spec arith_bor() -> ok.
arith_bor() ->
    emit_instruction(arith_bor, []).

%% @doc %comment-here%%
%%      <h4>opcode: 212</h4>
%% @end
-spec arith_bxor() -> ok.
arith_bxor() ->
    emit_instruction(arith_bxor, []).

%% @doc %comment-here%%
%%      <h4>opcode: 213</h4>
%% @end
-spec arith_bnot() -> ok.
arith_bnot() ->
    emit_instruction(arith_bnot, []).

%% @doc %comment-here%%
%%      <h4>opcode: 214</h4>
%% @end
-spec arith_bsl() -> ok.
arith_bsl() ->
    emit_instruction(arith_bsl, []).

%% @doc %comment-here%%
%%      <h4>opcode: 215</h4>
%% @end
-spec arith_bsr() -> ok.
arith_bsr() ->
    emit_instruction(arith_bsr, []).

%% @doc %comment-here%%
%%      <h4>opcode: 216</h4>
%% @end
-spec arith_rem() -> ok.
arith_rem() ->
    emit_instruction(arith_rem, []).

%% @doc %comment-here%%
%%      <h4>opcode: 217</h4>
%% @end
-spec comp_gt() -> ok.
comp_gt() ->
    emit_instruction(comp_gt, []).

%% @doc %comment-here%%
%%      <h4>opcode: 218</h4>
%% @end
-spec comp_lt() -> ok.
comp_lt() ->
    emit_instruction(comp_lt, []).

%% @doc %comment-here%%
%%      <h4>opcode: 219</h4>
%% @end
-spec comp_geq() -> ok.
comp_geq() ->
    emit_instruction(comp_geq, []).

%% @doc %comment-here%%
%%      <h4>opcode: 220</h4>
%% @end
-spec comp_leq() -> ok.
comp_leq() ->
    emit_instruction(comp_leq, []).

%% @doc %comment-here%%
%%      <h4>opcode: 223</h4>
%% @end
-spec comp_eqeq() -> ok.
exact_eqeq() ->
    emit_instruction(exact_eqeq, []).

%% @doc %comment-here%%
%%      <h4>opcode: 224</h4>
%% @end
-spec comp_neq() -> ok.
exact_neq() ->
    emit_instruction(exact_neq, []).

%% @doc test if TOS is an integer
%%      <h4>opcode: 225</h4>
%% @end
-spec test_integer() -> ok.
test_integer() ->
    emit_instruction(test_integer, []).

%% @doc test if TOS is a floating point value
%%      <h4>opcode: 226</h4>
%% @end
-spec test_float() -> ok.
test_float() ->
    emit_instruction(test_float, []).

%% @doc test if TOS is an integer or a floating point value
%%      <h4>opcode: 227</h4>
%% @end
-spec test_number() -> ok.
test_number() ->
    emit_instruction(test_number, []).

%% @doc test if TOS is an atom
%%      <h4>opcode: 228</h4>
%% @end
-spec test_atom() -> ok.
test_atom() ->
    emit_instruction(test_atom, []).

%% @doc test if TOS is an integer|float|binary|atom|reference|port|pid
%%      <h4>opcode: 229</h4>
%% @end
-spec test_constant() -> ok.
test_constant() ->
    emit_instruction(test_constant, []).

%% @doc test if TOS is a list (including empty list)
%%      <h4>opcode: 230</h4>
%% @end
-spec test_list() -> ok.
test_list() ->
    emit_instruction(test_list, []).

%% @doc test if TOS is a tuple
%%      <h4>opcode: 231</h4>
%% @end
-spec test_tuple() -> ok.
test_tuple() ->
    emit_instruction(test_tuple, []).

%% @doc test if TOS is a process id
%%      <h4>opcode: 232</h4>
%% @end
-spec test_pid() -> ok.
test_pid() ->
    emit_instruction(test_pid, []).

%% @doc test if TOS is a reference value
%%      <h4>opcode: 233</h4>
%% @end
-spec test_reference() -> ok.
test_reference() ->
    emit_instruction(test_reference, []).

%% @doc test if TOS is a port 
%%      <h4>opcode: 234</h4>
%% @end
-spec test_port() -> ok.
test_port() ->
    emit_instruction(test_port, []).

%% @doc make sure there are at least Need elements on stack
%%      <h4>opcode: 235</h4>
%% @end
-spec stack_need(Need::u16()) -> ok.
stack_need(Need) when ?is_u16(Need) ->
    emit_instruction(stack_need, [Need]).

%% @doc make sure there are at least Need elements on heap
%%      <h4>opcode: 236</h4>
%% @end
-spec heap_need(Need::u16()) -> ok.
heap_need(Need) when ?is_u16(Need) ->
    emit_instruction(head_need, []).

%% @doc %comment-here%%
%%      <h4>opcode: 237</h4>
%% @end

comp_eqeq() ->
    emit_instruction(comp_eqeq, []).

%% @doc %comment-here%%
%%      <h4>opcode: 238</h4>
%% @end

comp_neq() ->
    emit_instruction(neq, []).

%% @doc %comment-here%%
%%      <h4>opcode: 239</h4>
%% @end

-spec debug_info(M::atom(),F::atom(),A::u8()) -> ok.
debug_info(M,F,A) when is_atom(M), is_atom(F), ?is_u8(A) ->
    emit_instruction(debug_info, [A,M,F,0]).

%% @doc %comment-here%%
%%      <h4>opcode: 240</h4>
%% @end
-spec failFunction() -> ok.
failFunction() ->
    emit_instruction(failFunction, []).

%% @doc %comment-here%%
%%      <h4>opcode: 241</h4>
%% @end

test_binary() ->
    emit_instruction(test_binary, []).

%% @doc %comment-here%%
%%      <h4>opcode: 242</h4>
%% @end
-spec try_me_else(Label::integer()) -> ok.
try_me_else(Label) when is_integer(Label) ->
    emit_instruction(try_me_else, [Label]).

%% @doc %comment-here%%
%%      <h4>opcode: 243</h4>
%% @end
-spec goto(Label::integer()) -> ok.
goto(Label) when is_integer(Label) ->
    emit_instruction(goto, [Label]).

%% @doc %comment-here%%
%%      <h4>opcode: 244</h4>
%% @end
-spec call_local(Name::atom(), Arity::u8()) -> ok.
call_local(Name, Arity) when is_atom(Name), ?is_u8(Arity) ->
    emit_instruction(call_local, [Arity, {Name,Arity}]).

%% @doc %comment-here%%
%%      <h4>opcode: 245</h4>
%% @end
-spec enter_local(Name::integer(), Arity::u8()) -> ok.
enter_local(Name, Arity) when is_atom(Name), ?is_u8(Arity) ->
    emit_instruction(enter_local, [Arity, {Name,Arity}]).

%% @doc %comment-here%%
%%      <h4>opcode: 246</h4>
%% @end
-spec pushCatch(Label::integer()) -> ok.
pushCatch(Label) when is_integer(Label) ->
    emit_instruction(pushCatch, [Label]).

%% @doc %comment-here%%
%%      <h4>opcode: 247</h4>
%% @end
-spec wait1(Label::integer()) -> ok.
wait1(Label) when is_integer(Label) ->
    emit_instruction(wait1, [Label]).

%% @doc %comment-here%%
%%      <h4>opcode: 250</h4>
%% @end
-spec arith_neg() -> ok.
arith_neg() ->
    emit_instruction(neg, []).

%% @doc patch of erlang:apply(M, F, A) OP_enter_remote i.e remote call when
%%      Mod = erlang, Func = apply and Arity = 3, this is patched
%%      by the loader
%%      <h4>opcode: 251</h4>
%% @end
-spec apply_enter() -> ok.
apply_enter() ->
    emit_instruction(apply_enter, []).

%% @doc patch of erlang:apply(M, F, A) OP_call_remote i.e remote call when
%%      Mod = erlang, Func = apply and Arity = 3, this is patched
%%      by the loader
%%      <h4>opcode: 252</h4>
%% @end
-spec apply_call() -> ok.
apply_call() ->
    emit_instruction(apply_call, []).

%% @doc Call bif + return
%%      This op is normally a patch of OP_enter_remote by the 
%%      loader. The index is the index in the bif table jam_bif.tab
%%      <h4>opcode: 253</h4>
%% @end
-spec bif_enter(Arity::u8(), Index::u16()) -> ok.
bif_enter(Arity,Index) when ?is_u8(Arity), ?is_u16(Index) ->
    emit_instruction(bif_enter, [Arity,Index]).

%% @doc Call bif
%%      This op is normally a patch of OP_call_remote by the 
%%      loader. The index is the index in the bif table jam_bif.tab
%%      <h4>opcode: 254</h4>
%% @end
-spec bif_call(Arity::u8(), Index::u16()) -> ok.
bif_call(Arity, Index) when ?is_u8(Arity), ?is_u16(Index) ->
    emit_instruction(bif_call, [Arity,Index]).

%% @doc %comment-here%%
%%      <h4>opcode: 255</h4>
%% @end

die() ->
    emit_instruction(die, []).


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

exported_function(Mod, Name, Arity) when 
      is_atom(Mod), is_atom(Name), is_integer(Arity), Arity >= 0 ->
    count_function(),
    _Mi = insert_atom(Mod),
    _Fi = insert_atom(Name),
    %% insert_export(Name,Arity),
    nodebug_info(Mod, Name, Arity).

local_function(Mod, Name, Arity) when
      is_atom(Mod), is_atom(Name), is_integer(Arity), Arity >= 0 ->
    count_function(),
    _Mi = insert_atom(Mod),
    _Fi = insert_atom(Name),
    %% insert_local(Name,Arity,Entry),
    nodebug_info(Mod, Name, Arity).

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
    Ctx = jctx(),
    Pos = Ctx#jctx.pos,
    Entry = {L, Pos},
    {J, Labels} = insert_item(Ctx#jctx.labels, Entry),
    jctx(Ctx#jctx { labels = Labels, pos = Pos+3 }),
    J.

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
	     pos = 0,
	     atoms = #jctx_table{index=1},
	     literals = #jctx_table{},
	     imports = #jctx_table{},
	     exports = #jctx_table{},
	     locals = #jctx_table{},
	     lines = #jctx_table{index=1},
	     labels = #jctx_table{},
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

insert_item(Table=#jctx_table{map=Map,index=I}, Item) ->
    Map1 = Map#{ Item => I, I => Item },
    J = I+1,
    {I, Table#jctx_table{map=Map1,index=J}}.


find_by_item(Item, #jctx_table{map=Map}) ->
    maps:find(Item, Map).


emit_instruction(Mnemonic) when is_atom(Mnemonic) ->
    emit_instruction(Mnemonic, []);
emit_instruction({Mnemonic,Args}) when is_atom(Mnemonic), is_list(Args) ->
    emit_instruction(Mnemonic, Args).

emit_instruction(Mnemonic, Args) ->
    Opcode = maps:get(Mnemonic, opcode_map()),
    Ent = maps:get(Opcode, opcode_map()),
    Data = encode_args(Ent#opcode.argtypes, Args),
    io:format("mnemonic=~p, args=~p, opccode=~p, data=~p\n", 
	      [Mnemonic, Args, Opcode, Data]).

encode_args(Ts, As) ->
    encode_args_(Ts, As, []).
    
encode_args_([u8|Ts], [U|As], Acc) ->
    encode_args_(Ts, As, [<<U>> | Acc]);
encode_args_([u16|Ts], [U|As], Acc) ->
    encode_args_(Ts, As, [<<U:16>> | Acc]);
encode_args_([i24|Ts], [I|As], Acc) ->
    encode_args_(Ts, As, [<<I:24/signed>> | Acc]);
encode_args_([i32|Ts], [I|As], Acc) ->
    encode_args_(Ts, As, [<<I:32/signed>> | Acc]);

encode_args_([{u8,U}|Ts], As, Acc) ->
    encode_args_(Ts, As, [<<U>> | Acc]);
encode_args_([{u16,U}|Ts], As, Acc) ->
    encode_args_(Ts, As, [<<U:16>> | Acc]);

encode_args_([p16|Ts], [A|As], Acc) when is_atom(A) ->
    %% FIXME: add a patch
    encode_args_(Ts, As, [<<0:16>> < Acc]);
encode_args_([], [], Acc) ->
    lists:reverse(Acc).
