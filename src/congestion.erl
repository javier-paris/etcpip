%%%-------------------------------------------------------------------
%%% File    : congestion.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Congestion Control
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
-module(congestion).

-export([cgt_ctl/6]).

-define(max(X,Y), case X > Y of true -> X; false -> Y end).

% Updates congestion windows and slow start threshold
cgt_ctl(Rtcount, Snd_Nxt, Snd_Una, Smss, Cwnd, Ssthr) ->
    if
	Rtcount > 0 -> % There were retransmitions
	    FlightSize = seq:sub(Snd_Nxt, Snd_Una),
	    New_Ssthr = ?max(FlightSize/2, 2*Smss),
	    {Smss, New_Ssthr};
	true ->
	    {update_cwnd(Cwnd, Smss, Ssthr), Ssthr}
    end.

%% Updates the value of cwnd when a new ack arrives
update_cwnd(Cwnd, Smss, Ssthr) ->
    if
	Cwnd > Ssthr -> % Congestion avoidance
	    Cwnd + Smss*Smss/Cwnd;
	true -> % Slow start
	    Cwnd + Smss
    end.
