%%%-------------------------------------------------------------------
%%% File    : packet_check.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Packet checker for udp and tcp.
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
-module(packet_check).

-import(checksum, [checksum/1, checksum_1/1]).
-export([check_packet/4, compute_checksum/5]).

-define(BIG_PACKET, 100).

check_packet(Src_Ip, Dst_Ip, Protocol, Packet) ->
    Size = size(Packet),
    Chk_Packet = build_checksum_packet(Src_Ip, Dst_Ip, Protocol, 
				       Packet, Size),
    R = if Size < ?BIG_PACKET ->
		checksum_1(Chk_Packet);
	   true ->
		checksum(Chk_Packet)
	end,
    case R of
	0 ->
	    ok;
	_X ->
	    {error, bad_checksum}
    end.

build_checksum_packet(Src_Ip, Dst_Ip, Protocol, Packet, Len) ->
    Pad = case Len rem 2 of   % It must have even length
	      0 -> % Even Length
		  <<>>;
	      1 -> % Odd Length
		  <<0:8/integer>>
	  end,
    [<<Src_Ip:32/big-integer,
       Dst_Ip:32/big-integer,
       0:8/integer,
       Protocol:8/integer,
       Len:16/big-integer>>,
     Packet,
     Pad].

compute_checksum(Src_Ip, Dst_Ip, Protocol, Packet, Size) ->
    Chk_Packet = build_checksum_packet(Src_Ip, Dst_Ip, Protocol, Packet, Size),
    checksum(Chk_Packet).
