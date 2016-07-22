%%%-------------------------------------------------------------------
%%% File    : close_wait.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp close wait connection state
%%%
%%% Created :  7 Sep 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(close_wait).

-export([recv/3, send/2, badack_action/3, nonewdata_action/3, 
	 newdata_action/3, data_action/2, fin_action/3,
	 out_order_action/3, queue/0, read/2, close/0]).

%%%%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    tcp_input:process_packet(Tcb, Pkt, close_wait, Writer).

%%%%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Tcb, {send, ack}) ->
    tcp_packet:send_packet(Tcb, ack);
send(Tcb, {send, data}) ->
    tcp_packet:send_packet(Tcb, data);
send(Tcb, {send, fin}) ->
    Data_Size = tcb:get_tcbdata(Tcb, sbufsize),
    if
	Data_Size == 0 ->
	    tcb:syncset_tcbdata(Tcb, state, last_ack),
	    tcp_packet:send_packet(Tcb, fin);
	true ->
	    tcp_packet:send_packet(Tcb, data),
	    tcb:set_tcbdata(Tcb, send_fin, 1)
    end;
send(Tcb, rto) ->
    tcp_packet:send_packet(Tcb, rto);
send(_, _) -> 
    ok.

%%%%%%%%%%%%%%%%%%%%% TCP INPUT CALLBACKS %%%%%%%%%%%%%%%%%%%%%

badack_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

nonewdata_action(_, _, _) ->
    ok.

newdata_action(_, _, _) ->
    no_data_action.

data_action(_, _) ->
    ok.

out_order_action(_, _, _) ->
    ok.

fin_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

%%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

queue() ->
    ok.

read(Tcb, Bytes) ->
    case tcb:get_tcbdata(Tcb, rbufsize) of
	0 ->
	    {error, connection_closing};
	Size ->
	    {ok, min(Size, Bytes)}
    end.

close() ->
    ok.
