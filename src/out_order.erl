%%%-------------------------------------------------------------------
%%% File    : out_order.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Out of order Packet management
%%%
%%% Created : 18 Nov 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(out_order).

-export([new/0, merge_data/2, get_out_order/2]).

new() ->
    [].

merge_data([], Elem) ->
    [Elem];
merge_data(List = [{Lseq, Lis_Fin, Ldata}|T], Elem={Seq, Is_Fin, Data}) ->
    Pkt_Nxt = seq:add(Seq, size(Data)),
    LPkt_Nxt = seq:add(Lseq, size(Ldata)),
    
    if 
	Pkt_Nxt == Lseq ->
	    [{Seq, Lis_Fin, <<Data/binary, Ldata/binary>>} | T];
	LPkt_Nxt == Seq ->
	    New_Data = <<Ldata/binary, Data/binary>>,
	    merge_data(T, {Lseq, Is_Fin, New_Data});
	true ->
	    case seq:lt(Pkt_Nxt, Lseq) of
		true ->
		    [Elem | List];
		false ->
		    [{Lseq, Lis_Fin, Ldata} | merge_data(T, Elem)]
	    end
    end.

get_out_order([], _) ->
    {none, []};
get_out_order([{Lseq, Is_Fin, Data} | T], Seq) ->
    case seq:le(Lseq, Seq) of
	true ->
	    {{Lseq, Is_Fin, Data}, T};
	false ->
	    {none, T}
    end.
