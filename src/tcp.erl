%%%-------------------------------------------------------------------
%%% File    : tcp.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp receive multiplexor
%%%
%%% Created : 11 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(tcp).

-export([start/0,recv/3,reader_init/0, new_mtu/3, dst_unr/3]).

-include("tcp_packet.hrl").

start() ->
    spawn(tcp, reader_init, []).

recv(Src_Ip, Dst_Ip, Data) ->
    catch tcp_reader ! {recv, Src_Ip, Dst_Ip, Data}.

new_mtu(Src_Ip, Dst_Ip, Data) ->
    catch tcp_reader ! {new_mtu, Src_Ip, Dst_Ip, Data}.

dst_unr(Src_Ip, Dst_Ip, Data) ->
    catch tcp_reader ! {dst_unr, Src_Ip, Dst_Ip, Data}.

reader_init() ->
    register(tcp_reader, self()),
    reader_loop().

reader_loop() ->
    receive
	{recv, Src_Ip, Dst_Ip, Data} ->
	    case catch tcp_packet:parse(Src_Ip, Dst_Ip, Data) of 
		{ok, Pkt} ->
		    demux_packet(Pkt);
		{error, Error} -> % Bad checksum
		    {error, Error};
		{'EXIT', _} ->
		    {error, badpacket}
	    end;
	{new_mtu, Src_Ip, Dst_Ip, {Data, MTU}} ->
	    <<Src_Port:16/big-integer, 
	      Dst_Port:16/big-integer, 
	      _/binary>> = Data,
	    case tcp_pool:get({Src_Ip, Src_Port, Dst_Ip, Dst_Port}) of
		{ok, Conn} ->
		    tcp_con:new_mtu(Conn, MTU);
		{error, Error} ->
		    {error, Error}
	    end;
	{dst_unr, Src_Ip, Dst_Ip, Data} ->
	    <<Src_Port:16/big-integer,
	      Dst_Port:16/big-integer,
	      _/binary>> = Data,
	    case tcp_pool:get({Src_Ip, Src_Port, Dst_Ip, Dst_Port}) of
		{ok, Conn} ->
		    tcp_con:dst_unr(Conn);
		{error, Error} ->
		    {error, Error}
	    end
    end,
    reader_loop().

demux_packet(Pkt) ->
    case tcp_pool:get({Pkt#pkt.dip, Pkt#pkt.dport, 
		       Pkt#pkt.sip, Pkt#pkt.sport}) of
	{ok, Conn} ->
	    tcp_con:recv(Conn, Pkt);
	{error, _} -> % Try to find a passive connection (Incoming syn?)
	    case tcp_pool:get({Pkt#pkt.dip, Pkt#pkt.dport}) of
		{ok, Conn} ->
		    tcp_con:recv(Conn, Pkt);
		{error, Error} ->
		    closed:recv(Pkt), % Send rst
		    {error, Error}
	    end
    end.
