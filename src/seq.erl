%%%-------------------------------------------------------------------
%%% File    : seq.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Sequence numbers operations
%%%
%%% Created : 18 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(seq).

-export([lt/2, le/2, gt/2, ge/2, add/2, sub/2, max/2, min/2]).

-define(INT32,4294967296).

lt(X, Y) ->
    ((X - Y) band 16#80000000) > 0.

le(X, Y) ->
    Z = X - Y,
    (Z == 0) orelse ((Z band 16#80000000) > 0).

gt(X, Y) ->
    not le(X, Y).

ge(X, Y) ->
    not lt(X, Y).

add(X, Y) ->
    (X + Y) rem ?INT32. 

sub(X, Y) ->
    (X - Y) rem ?INT32.

min(X, Y) ->
    case lt(X, Y) of
	true ->
	    X;
	_ ->
	    Y
    end.

max(X, Y) ->
    case lt(Y, X) of
	true ->
	    X;
	_ ->
	    Y
    end.
		  
