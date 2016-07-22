%%%-------------------------------------------------------------------
%%% File    : eth_port.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Ethernet Port driver
%%%
%%% Created :  2 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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
-module(eth_port).

-export([start/1, init/1, init_writer/2, send/1, get_stats/0, get_mtu/0]).

start(Iface) ->
    spawn_link(eth_port, init, [Iface]).

init(Iface) ->
    erl_ddll:load_driver("c_src", "eth_driver"),
    Port = open_port({spawn_driver, eth_driver},[binary]),
    port_control(Port, 0, Iface),
    spawn_link(eth_port, init_writer, [Port, self()]),
    register(port_reader, self()),
    reader_loop(Port).

init_writer(Port, CPid) ->
    register(port_writer, self()),
    writer_loop(Port, CPid).

send(Packet) ->
    port_writer ! {send, Packet}.

get_mtu() ->
    port_reader ! {get_mtu, self()},
    receive
	{mtu, Mtu} ->
	    {mtu, Mtu}
    end.

get_stats() ->
    port_reader ! {get_stats, self()},
    receive
    	{stats, Stats} ->
		{ok, Stats}
    end.

writer_loop(Port, CPid) ->
    receive 
	{send, Packet} ->
	    Port ! {CPid, {command, Packet}},
	    writer_loop(Port, CPid)
    end.

reader_loop(Port) ->
    receive
	{Port, {data, Data}} ->
	    eth:recv(Data),
	    reader_loop(Port);
	{get_stats, From} ->
	    Stats = port_control(Port, 1, []),
	    From ! {stats, Stats},
	    reader_loop(Port);
	{get_mtu, From} ->
	    List_Mtu = port_control(Port, 2, []),
	    <<Mtu:32/native-integer>> = list_to_binary(List_Mtu),
	    From ! {mtu, Mtu},
	    reader_loop(Port);
	_ ->
	    reader_loop(Port)
    end.
