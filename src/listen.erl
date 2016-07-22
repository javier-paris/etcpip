%%%-------------------------------------------------------------------
%%% File    : listen.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp Listen connection state
%%%
%%% Created :  3 Sep 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(listen).

-export([recv/3, send/2, queue/0, read/2, close/0]).

-include("tcp_packet.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    case Pkt#pkt.is_rst of % Discard rsts
	1 ->
	    ok;
	0 ->
	    process_ack(Tcb, Pkt, Writer)
    end.

process_ack(Tcb, Pkt, Writer) ->
    case Pkt#pkt.is_ack of % Send an rst to an ack packet
	1 ->
	    rst(Tcb, Pkt); 
	0 ->
	    process_syn(Tcb, Pkt, Writer)
    end.

process_syn(Tcb, Pkt, _Writer) ->
    case Pkt#pkt.is_syn of
	1 ->
	    {N_Tcb, N_Reader, N_Writer} = tcp_con:clone(Tcb),
	    tcb:syncset_tcbdata(Tcb, syn_queue, {N_Tcb, N_Reader, N_Writer}),
	    
	    tcb:set_tcbdata(N_Tcb, rsocket, {Pkt#pkt.sip, Pkt#pkt.sport}),
	    tcb:set_tcbdata(N_Tcb, rcv_nxt, seq:add(Pkt#pkt.seq, 1)),
	    tcb:set_tcbdata(N_Tcb, irs, Pkt#pkt.seq),
	    tcb:syncset_tcbdata(N_Tcb, state, syn_rcvd),
	    Socket = {Pkt#pkt.dip, Pkt#pkt.dport, 
		      Pkt#pkt.sip, Pkt#pkt.sport},
	
	    tcp_pool:add({connect, Socket}, {N_Tcb, N_Reader, N_Writer}),
	    tcp_con:send_packet(N_Writer, synack);
	0 ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(_, _) ->
    ok.

rst(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%

queue() ->
    {error, no_connection}.

read(_, _) ->
    {error, no_connection}.

close() ->
    ok.
