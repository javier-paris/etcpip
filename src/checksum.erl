%%%-------------------------------------------------------------------
%%% File    : checksum.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Ip Protocol Checksum
%%%
%%% Created :  4 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(checksum).

-export([checksum/1, checksum_1/1, start/0, init/0]).

-define(INT16MAX, 65535).

start() ->
    spawn(checksum, init, []).

init() ->
    erl_ddll:load_driver("c_src", "checksum"),
    Port = open_port({spawn, checksum}, [binary]),
    register(checksum, self()),
    loop(Port).

checksum(Packet) ->
    checksum ! {check, Packet, self()},
    receive
	{checksum, Checksum} ->
	    Checksum
    end.

loop(Port) ->
    receive
	{check, Packet, From} ->
	    [Low, High | []] = port_control(Port, 0, Packet),
	    From ! {checksum, High*256+Low}
    end,
    loop(Port).

checksum_1(BinList) when is_list(BinList) ->
    checksum_1(BinList, 0);
checksum_1(Bin) ->
    (bnot checksum_2(Bin, 0)) band ?INT16MAX.

checksum_1([], Csum) ->
    (bnot Csum) band ?INT16MAX;
checksum_1([Bin|T], Csum) ->
    checksum_1(T, checksum_2(Bin, Csum)).

checksum_2(Bin = <<N1:16/integer, N2:16/integer, N3:16/integer, 
		  N4:16/integer, Rem/binary>>, Csum) when size(Bin) >= 8 ->
    checksum_2(Rem, Csum+N1+N2+N3+N4);
checksum_2(<<Num:16/integer, Remainder/binary>>, Csum) ->
    checksum_2(Remainder, Csum + Num);
checksum_2(<<Num:8/integer>>, Csum) ->
    checksum_2(<<>>, Csum+(Num bsl 8));
checksum_2(<<>>, Csum) when Csum > ?INT16MAX ->
    Carry = Csum bsr 16,
    checksum_2(<<>>, (Csum band ?INT16MAX) + Carry);
checksum_2(<<>>, Csum) ->
    Csum.
