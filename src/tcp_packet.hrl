%%%-------------------------------------------------------------------
%%% File    : tcp_packet.hrl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Tcp Packet Info Structure
%%%
%%% Created : 17 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
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

-record(pkt, {
	      sip,     % Source Ip
	      dip,     % Destination Ip
	      sport,   % Source Port
	      dport,   % Destination Port
	      seq,     % Sequence Number
	      ack,     % Acknowledge number
	      is_urg,  % Urgent flag
	      is_ack,  % Ack flag
	      is_psh,  % Push flag
	      is_rst,  % Reset flag
	      is_syn,  % Syn flag
	      is_fin,  % Fin flag
	      window,  % Window size
	      urgent,  % Urgent pointer
	      mss,     % Maximum segment size
	      data,    % Packet Data
	      data_size}). % Size of data
