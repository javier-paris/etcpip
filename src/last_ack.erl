%%%-------------------------------------------------------------------
%%% File    : last_ack.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : 
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
-module(last_ack).

-export([recv/3, send/2, badack_action/3, newdata_action/3,
	 nonewdata_action/3, data_action/2, fin_action/3,
	 out_order_action/3, queue/0, read/2, close/0]).

%%%%%%%%%%%%%%%%%%%%%% READER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Tcb, Pkt, Writer) ->
    tcp_input:process_packet(Tcb, Pkt, last_ack, Writer).

%%%%%%%%%%%%%%%%%%%%%% WRITER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Tcb, rto) -> % Only send retransmitions in this state
    tcp_packet:send_packet(Tcb, rto);
send(_, _) -> 
    ok.

%%%%%%%%%%%%%%%%%% TCP INPUT CALLBACKS %%%%%%%%%%%%%%%%%%%%

badack_action(_, _, Writer) ->
    tcp_con:send_packet(Writer, ack).

nonewdata_action(_, _, _) ->
    ok.

newdata_action(Tcb, _, _) ->
    {Snd_Una, Snd_Nxt, _, _} = tcb:get_tcbdata(Tcb, snd),
    if
	Snd_Una == Snd_Nxt -> % All data acked, close
	    tcp_con:close_connection(Tcb),
	    ok;
	true ->
	    no_data_action
    end.

data_action(_, _) ->
    ok.

out_order_action(_, _, _) ->
    ok.

fin_action(_, _, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%% USER COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%

queue() ->
    {error, connection_closing}.

read(_, _) ->
    {error, connection_closing}.

close() ->
    {error, connection_closing}.
