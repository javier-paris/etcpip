%%%-------------------------------------------------------------------
%%% File    : iss.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Initial Segment number generator
%%%
%%% Created : 12 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(iss).

-export([start/0,init/0,get_iss/0]).

-define(TCP_MAX_ISS, 4294967295).
-define(TCP_ISS_INC, 64000).

%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    spawn(iss, init, []).

get_iss() ->
    iss ! {get_iss, self()}, 
    receive
	{iss, Iss} ->
	    Iss
    end.

%%%%%%%%%%%%%%%%%%%%%%% Iss Server %%%%%%%%%%%%%%%%%%%%%%%%

init() ->
    {_, Secs, Msecs} = erlang:timestamp(),
    Iss = Secs*Msecs rem ?TCP_MAX_ISS,
    register(iss, self()),
    loop(Iss).

loop(Iss) ->
    receive
	{get_iss, From} ->
	    From ! {iss, Iss},
	    loop((Iss + ?TCP_ISS_INC) rem ?TCP_MAX_ISS);
	_ ->
	    loop(Iss)
    end.
