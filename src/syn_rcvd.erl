%%%-------------------------------------------------------------------
%%% File    : syn_rcvd.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp Syn Received connection state
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
-module(syn_rcvd).

-export([recv/3, send/2, badack_action/3, nonewdata_action/3, 
	 newdata_action/3, data_action/2, fin_action/3, 
	 out_order_action/3, queue/0, read/2, close/0]).

%%%%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    tcp_input:process_packet(Tcb, Pkt, syn_rcvd, Writer).

%%%%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Tcb, {send, synack}) ->
    tcp_packet:send_packet(Tcb, synack);
send(Tcb, rto) ->
    tcp_packet:send_packet(Tcb, rto);
send(_, _) -> % Ignore everything else
    ok.

%%%%%%%%%%%%%%%%%%%% TCP INPUT CALLBACKS %%%%%%%%%%%%%%%%%%%%%%

badack_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, rst). % It should send the data from the packet. TODO

nonewdata_action(_, _, Writer) -> % Idem
    tcp_con:send_packet(Writer, rst),
    {error, badack}.

newdata_action(Tcb, _, _) ->
    tcb:syncset_tcbdata(Tcb, state, established).

data_action(Tcb, Data) ->
    tcb:set_tcbdata(Tcb, rdata, Data),
    ok.

out_order_action(Tcb, Data, Writer) ->
    tcb:set_tcbdata(Tcb, out_order, Data),
    tcp_con:send_packet(Writer, ack).

fin_action(Tcb, Rcv_Nxt, Writer) ->
    tcb:set_tcbdata(Tcb, rcv_nxt, seq:add(Rcv_Nxt, 1)),
    tcb:syncset_tcbdata(Tcb, state, close_wait),
    tcp_con:send_packet(Writer, ack).

%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

queue() ->
    ok.

read(_, Bytes) ->
    {ok, Bytes}.

close() ->
    ok.
