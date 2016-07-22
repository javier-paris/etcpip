%%%-------------------------------------------------------------------
%%% File    : closed.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp Closed connection State
%%%
%%% Created : 17 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(closed).

-export([recv/1, send/2]).

-include("tcp_packet.hrl").

recv(Pkt) -> % Should build a fake tcb instead ?
    case Pkt#pkt.is_rst of
	1 ->
	    ok;
        0 ->
	    Seq = case Pkt#pkt.is_ack of
		      0 ->
			  0;
		      1 ->
			  Pkt#pkt.ack
		  end,
	    Ack = seq:add(Pkt#pkt.seq, size(Pkt#pkt.data) + 1),
	    rst(Pkt#pkt.dip, Pkt#pkt.dport, Pkt#pkt.sip, 
		Pkt#pkt.sport, Seq, Ack)
    end.

send(Tcb, {send, syn}) ->
    tcb:syncset_tcbdata(Tcb, state, syn_sent),
    tcp_packet:send_packet(Tcb, syn);
send(_, _) ->
    {error, unknown_message}.

rst(Src_Ip, SPort, Dst_Ip, DPort, Seq, Ack) ->
    Pkt = #pkt{
      sip   = Src_Ip,
      dip   = Dst_Ip,
      sport = SPort,
      dport = DPort,
      seq   = Seq,
      ack   = Ack,
      is_urg= 0,
      is_ack= 1,
      is_psh= 0,
      is_rst= 1,
      is_syn= 0,
      is_fin= 0,
      window= 0,
      urgent= 0,
      mss   = 0,
      data  = <<>>,
      data_size = 0},
    tcp_packet:send_packet(Pkt).

