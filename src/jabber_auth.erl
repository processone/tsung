%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%% 

-module(jabber_auth).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("ts_profile.hrl").
-include("ts_jabber.hrl").

-export([get_client/2, get_random_message/1]).


get_random_message (Args) ->
    jabber_common:get_random_message(Args).


%%generate a client session which only connects clients send one
%%message and wait for all the other client to be connected before
%%quitting

get_client(N, Id) ->
    [Message] = jabber_common:get_random_params(infinity, 1 ,
												?config(messages_size),'chat'),
    [#message{ack = no_ack, thinktime=1000, param = #jabber {type = 'connect'}}, 
     #message{ack = local, thinktime=infinity, 
			  param = #jabber {type = 'authenticate', id = Id}},
     #message{ack = no_ack, thinktime=?config(presence_delay), 
			  param = #jabber {type = 'presence'}}] ++
		[Message#message{ack = global}] ++
		[#message{ack = local, thinktime = infinity, param = #jabber {type = 'close'}}].
