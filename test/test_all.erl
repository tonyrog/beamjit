%% module that implete all? opcodes
-module(test_all).
-on_load(init/0).
-export([int/1, flt/1, gc_bif/2, bif/1]).
-export([pbin/4, gbin/1, gbin2/1, sbin/1, lbin/1, tqbin/1]).
-export([pbits/4]).
-export([icall/1, xcall/1, ocall/1, hcall/1, tcall/1, 
	 fcall/1, acall/1, aacall/1]).
-export([cif/2, ccase/1, cmesg/1, ctry/1, ccatch/1]).
-export([heap/1, stk/1, swp/1, tpl/1]).
-export([bug/1]).

-record(s, {a,b,c,d,e,f,g}).

init() ->
    ok.

int(X) when is_integer(X) ->
    -(((X*(X+X)-X) div X) rem X);
int(Y) ->
    int(trunc(Y)).

flt(X) when is_float(X) ->
    -((X*(X+X)-X) / X);
flt(Y) ->
    X = float(Y),
    -((X*(X+X)-X) / X).

gc_bif(X,Y) ->
    S = self(),
    A = round(X),
    T = Y#{ a => 12, b := 13 },
    C = binary_part(pbin(X,1.2,13,<<>>), 2, 5),
    K = maps:is_key(c, T),
    #{ c := C1 } = T,
    D = case T of
	    #{ d := _ } -> true;
	    _ -> false
	end,
    {S,A,T,C,K,C1,D}.

bif(X) ->
    A = element(1, X),
    T = tl(A),
    {A,T}.

pbin(X,Y,Z,B) ->
    <<B/binary,X:16/native, Y:32/float, Y:32/signed-little, 
      Z/utf8, Z/utf16, Z/utf32, X:32/little>>.

pbits(X,Y,Z,B) ->
    <<X:1, Y:13, Z:17, B/bitstring>>.

gbin(<<X1:16, X2:4/unit:16, Y1:32/float, Y2:32, 
       Z1/utf8, Z2/utf16, Z3/utf32, X1:32/little,Tail/binary>>) ->
    [{X1,X2, Y1,Y2, Z1,Z2,Z3} | ?MODULE:gbin(Tail)].

gbin2(Binary) when byte_size(Binary) > 64 ->
    <<A:16/binary, B:16/binary, _/binary>> = Binary,
    {A,B}.

sbin(<<_:16, _:4/unit:16, Y1:32/float, Y2:32, _/utf8, 1:8, _/utf16, 1:8, _/utf32, 2:32/little>>) ->
    {Y1, Y2}.

lbin(List) when is_list(List) ->
    lbin_(List, 0, <<>>).

lbin_([H|T], Sum, AccBin) ->
    lbin_(T, Sum+H, <<H,AccBin/binary>>);
lbin_([], Sum, AccBin) ->
    <<Sum,AccBin/binary>>.

tqbin(L) ->
    << <<X:2>> || X <- L >>.

icall(X) ->
    Y = int(X),
    flt(Y).

hcall(L=[X|_]) when X > 0 ->
    [A|_] = L,
    A.

tcall(L=[_|_]) ->
    [_|Y] = L,
    Y.


xcall(X) ->
    Y = ?MODULE:int(X),
    ?MODULE:flt(Y).

acall(X) ->
    Y = apply(?MODULE, int, [X]),
    apply(?MODULE, flt, [Y]).

aacall(X) ->
    Y = X:int(X),
    X:flt(Y).

ocall(X) ->
    icall(X).

fcall(X) ->
    F = fun (Xi) -> Xi + icall(1) end,
    G = fun erlang:abs/1,
    {G, F, G(F(X))}.
    
heap(X) ->
    Lx3 = [X],
    Tx3 = {X},
    {Lx3,Tx3}.    

cif(X,Y) ->
    if X =:= true -> 1;
       X =:= false -> 1;
       is_pid(X); is_port(X), is_reference(X) -> 2;
       is_atom(X) -> 3;
       is_number(X), X < 10 -> 4;
       is_number(X), X >= 100 -> 5;
       X == 30 -> 6;
       (X+Y) /= 40 -> 7;
       (X-Y) =/= 60 -> 9;
       X =:= 50 -> 8;
       is_integer(X) -> 10;
       is_float(X) -> 11;
       is_number(Y) -> 12;
       X =:= [] -> 13;
       is_list(X) -> 14; 
       is_binary(X) -> 15;
       is_tuple(X) -> [16|X#s{a=16}];
       tuple_size(X) =:= 17 -> 17;
       is_function(X,2) -> X(X,X);
       is_function(X) -> X(X);
       is_bitstring(X) -> 18
       %% is_constant(X) -> 16
    end.

ccase(X) ->
    case X of
	1 -> a;
	Y when is_list(Y) -> lists:reverse(Y);
	{a,b} -> d;
	{a,b,c} -> setelement(3, X, f);
	B when tuple_size(B) =:= 4 -> setelement(1, B, 0);
	{a,B,c,d,e} -> {a,B,c};
	[A,b,c] -> A;
	[a,B,c,d] -> [B,B,c,d];
	[A|B] when is_integer(A) -> {A,B};
	[_|_] -> c;
	#{ a := A } -> A
    end.

cmesg(X) ->
    Ref = make_ref(),
    self() ! a,
    receive
	Ref -> r
    after 10 ->
	    receive
		1 -> self() ! b;
		X -> b;
		Y when is_list(Y) -> lists:reverse(Y)
	    end
    end.

ctry(X) ->
    try icall(X) of
	A when A < 12 -> icall(A);
	B when B > 12 -> icall(B)
    catch
	error:badarg:Stack -> {ok,error,Stack};
	error:Reason:Stack -> erlang:raise(error, Reason,Stack);
	throw:_ -> {ok,throw}
    after
	erlang:display(X),
	erlang:raise(error, badarg, [])
    end.

ccatch(X) ->
    {catch icall(X), X}.

stk(X) ->
    A = X+1,
    B = 2*X,
    C = X+3,
    D = xcall(A+B+C),
    E = xcall(D+hcall(L=[A,B,C,A])),
    F = X+2,
    G = 3*X,
    H = xcall(F+G+B),
    {D,E,H,L}.

swp(X) ->
    A = X+12,
    B = X*13,
    {swap(A,B),
     swap(B,A)}.

swap(X,Y) ->
    {X,Y}.

tpl(S=#s{}) ->
    Y = S#s { a=1, b=1 },
    Y#s { c = 2}.

bug(X) ->
    A = element(X, 1),
    A.

    

