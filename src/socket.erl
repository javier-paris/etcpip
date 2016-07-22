%%%-------------------------------------------------------------------
%%% File    : socket.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Socket interface for Tcp/Ip
%%%
%%% Created : 14 Sep 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(socket).

-export([start/0, start/1, open/3, open/4, listen/1, accept/1, recv/2, send/2, 
	 send/4, close/1, string_to_ip/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%% USER API %%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    init("conf").

start(Conf) ->
    init(Conf).

open(tcp, Dst_Ip, Dst_Port) ->
    tcp_con:usr_open(Dst_Ip, Dst_Port).

open(udp, Lc_Port, Dst_Ip, Dst_Port) -> %% Udp
    udp:usr_open(Lc_Port, Dst_Ip, Dst_Port).

listen(Src_Port) ->
    tcp_con:usr_listen(Src_Port).

accept(ListenConn) ->
    tcp_con:usr_accept(ListenConn).

recv(Conn, Bytes) ->
    tcp_con:usr_recv(Conn, Bytes).

send(Conn, Data) ->
    tcp_con:usr_send(Conn, Data).

send(Src_Port, Dst_Ip, Dst_Port, Data) -> %% Udp
    udp:send(Dst_Ip, Dst_Port, Src_Port, Data).

close(Conn) ->
    tcp_con:usr_close(Conn).

string_to_ip(Ip) ->
    T = string:tokens(Ip, "."),
    lists:foldl(fun (N, Acc) -> {N2, _} = string:to_integer(N), Acc*256+N2 end, 0, T).
    
%%%%%%%%%%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%

init(Conf) ->
    case file:consult(Conf) of
	{ok, Terms} ->
	    {value, {ip, Ip}} = lists:keysearch(ip, 1, Terms),
	    {value, {netmask, NetMask}} = lists:keysearch(netmask, 1, Terms),
	    {value, {gateway, GateWay}} = lists:keysearch(gateway, 1, Terms),
	    {value, {mac, Mac}} = lists:keysearch(mac, 1, Terms),
	    {value, {iface, Iface}} = lists:keysearch(iface, 1, Terms),
	    erl_ddll:start(),
	    eth_port:start(Iface),
	    eth:start(Mac),
	    arp:start(Ip, Mac),
	    checksum:start(),
	    ip:start(Ip, NetMask, GateWay),
	    icmp:start(),
	    udp:start(Ip),
	    tcp_pool:start(Ip),
	    iss:start(),
	    tcp:start();
	{error, Error} ->
	    {error, Error}
    end.
