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

-module(jabber_roster).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("../include/ts_profile.hrl").
-include("../include/ts_jabber.hrl").

-export([get_client/2, get_random_message/1]).

get_random_message(Args) ->
    jabber_common:get_random_message(Args).

get_client(N, Id)->
    [#message{ack = no_ack, thinktime=3000, param = #jabber {type = 'connect'}}, 
     #message{ack = ?config(messages_ack), thinktime=infinity, param = #jabber {type = 'authenticate', id = Id}},
     #message{ack = no_ack, thinktime=random:uniform(?config(presence_delay)), param = #jabber {type = 'presence'}}] ++
		set_roster_params(?config(n_roster_clients), both,  Id) ++
		get_roster_params(N, Id) ++
	[ #message{ack = local, thinktime = infinity, param = #jabber {type = 'close'}}].


get_roster_params(1, Id, L)  ->
   L ++ [#message{ack = no_ack,
		  thinktime =  ?config(messages_last_time),
		  param = #jabber {
		    id = Id,
		    type = 'iq:roster:get'}}];
get_roster_params(N, Id, L)  ->
    get_roster_params(N-1, Id, [#message{ack = no_ack,
					 thinktime = round(ts_stats:exponential(?messages_intensity)),
					 param = #jabber {
					   id = Id,
					   type = 'iq:roster:get'}}| L]).

%%To build a list in order to establish roster of this client.
get_roster_params(N, Id) when integer(N), N>=0 ->
    get_roster_params(N, Id, []).

%%
set_roster_params(0, Status, Id, L)  ->
    L;
set_roster_params(N, Status, Id, L)  ->
    case Status of 
		online ->
			Roster1 = ts_user_server:get_one_connected(Id),
			Roster2 = ts_user_server:get_one_connected(Id);
		offline ->
			Roster1 = ts_user_server:get_offline(),
			Roster2 = ts_user_server:get_offline();
		both ->
			Roster1 = ts_user_server:get_one_connected(Id),
			Roster2 = ts_user_server:get_offline()
    end,
	   
    List = [#message{ack = no_ack,
		     thinktime = round(ts_stats:exponential(?setroster_intensity)),
		     param = #jabber { dest = Roster1,  type = 'iq:roster:set'}},
	    #message{ack = no_ack,
		     thinktime = round(ts_stats:exponential(?setroster_intensity)),
		     param = #jabber { dest = Roster1,  type = 'presence:roster'}},
	    #message{ack = no_ack,
		     thinktime = round(ts_stats:exponential(?setroster_intensity)),
		     param = #jabber { dest = Roster2,  type = 'iq:roster:set'}},
	    #message{ack = no_ack,
		     thinktime = round(ts_stats:exponential(?setroster_intensity)),
		     param = #jabber { dest = Roster2,  type = 'presence:roster'}}],
    set_roster_params(N-2, Status, Id, List ++ L).

%%To build a list of messages to retrieve roster of Id client
set_roster_params(N, Status, Id) when integer(N), N>=0 ->
    set_roster_params(N, Status, Id, []).
