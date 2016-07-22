%%%-------------------------------------------------------------------
%%% File    : icmp.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Icmp Protocol implementation
%%%
%%% Created :  4 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
%%%
%%%
%%% etcpip, Copyright (C) 2004 Javier Paris 
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, see <http://www.gnu.org/licenses/>.
%%%
%%%-------------------------------------------------------------------
-module(icmp).

-import(checksum,[checksum/1]).
-export([start/0,init_reader/0, init_writer/0, recv/2, send/2]).

-define(ICMP_ECHO_REPLY, 0).
-define(ICMP_DESTINATION_UNREACHABLE, 3).
-define(ICMP_SOURCE_QUENCH, 4).
-define(ICMP_REDIRECT, 5).
-define(ICMP_ECHO_REQUEST, 8).
-define(ICMP_TIME_EXCEEDED, 11).
-define(ICMP_PARAMETER_PROBLEM, 12).
-define(ICMP_TIMESTAMP_REQUEST, 13).
-define(ICMP_TIMESTAMP_REPLY, 14).
-define(ICMP_INFORMATION_REQUEST, 15).
-define(ICMP_INFORMATION_REPLY, 16).

-define(ICMP_DST_UNR_NET, 0).
-define(ICMP_DST_UNR_HOST, 1).
-define(ICMP_DST_UNR_PROTOCOL, 2).
-define(ICMP_DST_UNR_PORT, 3).
-define(ICMP_DST_UNR_FRAGMENTATION, 4).
-define(ICMP_DST_UNR_SOURCE_ROUTE, 5).


start() ->
    init().

init() ->
    spawn(icmp, init_writer, []),
    spawn(icmp, init_reader, []).

init_reader() ->
    register(icmp_reader, self()),
    reader_loop().

init_writer() ->
    register(icmp_writer, self()),
    writer_loop().

recv(Src_Ip, Data) ->
    icmp_reader ! {recv, Src_Ip, Data}. 

send(Type, Data) ->
    icmp_writer ! {send, Type, Data}.

reader_loop() ->
    receive
	{recv, Src_Ip, Data} ->
	    decode(Src_Ip, Data)
    end,
    reader_loop().

writer_loop() ->
    receive
	{send, echo, {Type, Dst_Ip, Identifier, Sequence, Data}} ->
	    Packet = create_echo(Type, Identifier, Sequence, Data),
	    ip:send(Packet, size(Packet), icmp, Dst_Ip);
	_ ->
	    ok
    end,
    writer_loop().

decode(Src_Ip, Packet) ->
    <<Type:8/integer, 
      Code:8/integer, 
     _:2/binary,
     Remainder/binary>> = Packet, 
    case Type of
	?ICMP_ECHO_REQUEST ->
	    case decode_echo(Remainder) of
		{ok, Identifier, Sequence, Data} ->
		    send(echo, {reply, Src_Ip, Identifier, 
				Sequence, Data});
		_ ->
		    {error, bad_echo_request}
	    end;
	?ICMP_ECHO_REPLY ->
	    case decode_echo(Remainder) of
		{ok, _Identifier, Sequence, _Data} ->
		    io:format("Ping from ~w:~w~n",[Src_Ip, Sequence]);
		_ ->
		    {error, bad_echo_reply}
	    end;
	?ICMP_DESTINATION_UNREACHABLE ->
	    case Code of
		?ICMP_DST_UNR_FRAGMENTATION -> % For PMTU
		    <<_:2/binary, MTU:16/big-integer,
		     IP_Packet/binary>> = Remainder,
		    ip:change_mtu(IP_Packet, MTU);
		_ ->                           % Just notify the error
		    <<_:4/binary, IP_Packet/binary>> = Remainder,
		    ip:dst_unreachable(IP_Packet)
	    end;
	_ ->
	    {error, notsupported}
    end.
     
decode_echo(Packet) ->
    <<Identifier:16/big-integer, Sequence:16/big-integer, 
     Data/binary>> = Packet,
    {ok, Identifier, Sequence, Data}.

create_echo(Type, Identifier, Sequence, Data) ->
    Num_Type = type(Type),
    Packet = <<Num_Type:8/integer, 0:8/integer, 0:16/big-integer,
	      Identifier:16/big-integer, Sequence:16/big-integer,
	      Data/binary>>,
    Checksum = checksum(Packet),
    <<Pre_Checksum:2/binary,_:2/binary,
     Post_Checksum/binary>> = Packet,
    <<Pre_Checksum/binary, Checksum:16/integer,
     Post_Checksum/binary>>.

type(request) -> ?ICMP_ECHO_REQUEST;
type(reply) -> ?ICMP_ECHO_REPLY.
    
