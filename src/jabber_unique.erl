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

-module(jabber_unique).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("../include/ts_profile.hrl").
-include("../include/ts_jabber.hrl").

-export([get_client/2, get_random_message/1]).


get_random_message(Args) ->
    jabber_common:get_random_message(Args).


%% generate a client session for one first client jabber who never disconnects
%% currently, parameters are included from profile.hrl
jabber_first()->
    {Id, _} = ts_user_server:get_first(),
	    io:format("First client ~w~n", [Id]),
    List_Fin = [#message{ack = no_ack, thinktime=3000, param = #jabber {type = 'connect'}}, 
		#message{ack = ?config(messages_ack), thinktime=infinity, param = #jabber {type = 'authenticate', id = Id}},
		#message{ack = no_ack, thinktime=infinity, param = #jabber {type = 'presence'}}],
    List_Fin.


%Send only messages to one client (6.3)
get_client(N, Id)->
    case  ts_user_server:get_first() of
	{Fid, not_connected} ->
	    ts_user_server:connect_first(),
	    jabber_first();
	{Fid, connected} ->
	    get_client2(N, Id)
    end.

%%Create a client session where all messages will be sent to an unique client connected with first_jabber_client
get_client2(N, Id)->
    List_Fin = [#message{ack = no_ack, thinktime=3000, param = #jabber {type = 'connect'}}, 
		#message{ack = ?config(messages_ack), thinktime=infinity, param = #jabber {type = 'authenticate', id = Id}},
		#message{ack = no_ack, thinktime=random:uniform(2000), param = #jabber {type = 'presence'}}] ++
		get_unique_params(?messages_intensity,
										 N,
										 ?config(messages_size),'chat', Id) ++
	[ #message{ack = local, thinktime = infinity, param = #jabber {type = 'close'}}],
    List_Fin.

%%%
get_unique_params(Intensity, 1, Size, Type, Id, L) -> 
    {Fid, _} = ts_user_server:get_first(),
    L ++ [#message{ ack = no_ack, 
		    thinktime = ?config(messages_last_time),
		    param = #jabber {size=Size, 
				     type=Type,
				     id =Id,
				     dest = Fid}}];


get_unique_params(Intensity, N, Size, Type, Id, L)  ->
    {Fid, _} = ts_user_server:get_first(),
    get_unique_params(Intensity, N-1, Size, Type, Id,
		      [#message{ ack = no_ack, 
				 thinktime = round(ts_stats:exponential(Intensity)),
				 param = #jabber {size=Size, 
						  type=Type, 
						  id = Id,
						  dest = Fid}
				}
		       | L]).
get_unique_params(Intensity, N, Size, Type, Id) when integer(N), N >= 0 ->
    get_unique_params(Intensity, N, Size, Type, Id ,[]).

