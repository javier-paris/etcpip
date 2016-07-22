%%%-------------------------------------------------------------------
%%% File    : rtt.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Round trip time and retransmition timeout estimation
%%%
%%% Created : 19 Nov 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(rtt).

-export([check_rttimer/7, set_rttimer/3]).

-define(DEFAULT_G, 1). % clock precision in ms
-define(MIN_RTO, 1000). % one second

%% Check if the packet used to measure rtt has been acked and update rto if so.
% Returns {Rttimer, Rtseq, Rto, Srtt, Rttvar}

check_rttimer(Rtseq, Rtcount, Rttimer, Srtt, Rttvar, Rto, Snd_Una) -> 
    if
	(Rtseq >= 0) and (Rtcount == 0) ->
	    case seq:lt(Rtseq, Snd_Una) of
		true ->
		    Rtt = timer:now_diff(erlang:timestamp(), Rttimer) / 1000,
		    {N_Rto, N_Srtt, N_Rttvar} = compute_rto(Srtt, Rttvar, Rtt),
		    {-1, -1, N_Rto, N_Srtt, N_Rttvar};
		false ->
		    {Rttimer, Rtseq, Rto, Srtt, Rttvar}
	    end;
	true ->
	    {Rttimer, Rtseq, Rto, Srtt, Rttvar}
    end.

%% Compute retransmit timeout from a new round trip time measure.
compute_rto(Srtt, Rttvar, Rtt) ->
    case Srtt of
	-1 -> % first time
	    N_Srtt = Rtt,
	    N_Rttvar = Rtt / 2,
	    Rto = max(N_Srtt + max(?DEFAULT_G, 4*N_Rttvar), ?MIN_RTO),
	    {Rto, N_Srtt, N_Rttvar};
	_ ->
	    N_Rttvar = 0.75*Rttvar + 0.25*abs(Srtt - Rtt),
	    N_Srtt = 0.875*Srtt + 0.125 * Rtt,
	    Rto = max(N_Srtt + max(?DEFAULT_G, 4*N_Rttvar), ?MIN_RTO),
	    {Rto, N_Srtt, N_Rttvar}
    end.

%% Check if we are alredy measuring rtt, and set a timer if not.
set_rttimer(Rttimer, Snd_Nxt, RtSeq) ->
    if 
	Rttimer == -1 ->
	    {erlang:timestamp(), Snd_Nxt};
	true ->
	    {Rttimer, RtSeq}
    end.
