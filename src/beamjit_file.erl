%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Read/Write beam file
%%% @end
%%% Created : 12 Jun 2022 by Tony Rogvall <tony@rogvall.se>

-module(beamjit_file).

-export([fold_chunks/3]).  %% main read api
-export([load/1]).         %% lazy api
%% "low" level api
-export([open/2, close/1, read_chunk/1, write_chunk/2]).
-export([read_data/2, write_data/2]).
-export([open_ram/1, open_ram/2]).
-export([fd/1]).

%%-define(verbose(F,A), io:format((F),(A))).
-define(verbose(F,A), ok).

-record(beam_fd,
	{
	 fd,
	 size
	}).

-define(align(N,M), ((M) * (((N)+((M)-1)) div (M)) )).

open(Filename,Mode) ->
    case file:open(Filename, [binary|Mode]) of
	{ok,Fd} ->
	    {ok, #beam_fd{fd=Fd}};
	Error ->
	    Error
    end.

open_ram(Mode) ->
    open_ram([], Mode).
open_ram(InitialData, Mode) ->
    {ok,Fd} = ram_file:open(InitialData, [binary|Mode]),
    {ok,#beam_fd{fd=Fd,size=iolist_size(InitialData)}}.
    
fd(#beam_fd{fd=Fd}) ->
    Fd.
		
close(#beam_fd{fd=Fd}) ->
    file:close(Fd).

read_chunk(#beam_fd{fd=Fd}) ->
    case file:read(Fd, 8) of
	{ok, <<N,A,M,E, Size:32/integer>>} ->
	    Len = ?align(Size, 4),
	    ?verbose("beamjit_file:read_chunk ~p, size=~w, len=~w\n", 
		     [<<N,A,M,E>>, Size, Len]),
	    case file:read(Fd, Len) of
		{ok, <<Chunk:Size/binary,_Pad/binary>>} ->
		    {ok,{<<N,A,M,E>>,Chunk}};
		{ok, _} ->
		    {error, chunk_truncated}
	    end;
	{ok, _} ->
	    {error, chunk_truncated};
	Error ->
	    Error
    end.

read_data(#beam_fd{fd=Fd}, Len) ->
    file:read(Fd, Len).

write_chunk(BeamFd, {<<N,A,M,E>>,Chunk}) ->
    Size = byte_size(Chunk),
    Len = ?align(Size, 4),
    Pad = <<0:(Len-Size)/unit:8>>,
    write_data(BeamFd, <<N,A,M,E, Size:32, Chunk/binary, Pad/binary>>).

write_data(#beam_fd{fd=Fd}, Data) when is_list(Data); is_binary(Data) ->
    file:write(Fd, Data).


fold_chunks(Data, Fun, Acc) when is_binary(Data) ->
    case open_ram(Data, [read]) of
	{ok,Fd} ->
	    fold_chunks_fd_(Fd, Fun, Acc);
	Error ->
	    Error
    end;
fold_chunks(Filename, Fun, Acc) ->
    case open(Filename,[read]) of
	{ok,Fd} ->
	    fold_chunks_fd_(Fd, Fun, Acc);
	Error ->
	    Error
    end.

fold_chunks_fd_(Fd, Fun, Acc) ->
    case read_data(Fd, 12) of
	{ok,<<"FOR1", _Size:32, "BEAM">>} ->
	    try fold_chunks_(Fd, Fun, Acc) of
		Acc1 -> Acc1
	    after
		close(Fd)
	    end;
	{ok, _} ->
	    close(Fd),
	    {error, bad_magic};
	Error ->
	    close(Fd),
	    Error
    end.

fold_chunks_(Fd, Fun, Acc) ->
    case read_chunk(Fd) of
	{ok, {Name, Chunk}} ->
	    Acc1 = Fun(Name, Chunk, Acc),
	    fold_chunks_(Fd, Fun, Acc1);
	eof ->
	    Acc;
	Error ->
	    Error
    end.

load(Filename) ->
    lists:reverse(
      fold_chunks(Filename,
		  fun(Name, Chunk, Acc) ->
			  [{Name,Chunk}|Acc]
		  end, [])).
