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

-module(jabber_common).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-export([connect/0,   close/0, 
		 get_random_params/4,  
		 get_random_message/1
		]). 

-export([auth/0, 
	 message/2,
	 message/3, 
	 presence/0, 
	 presence/2,
	 registration/0,
	 requete/3
		]).

-include("../include/ts_profile.hrl").
-include("../include/ts_jabber.hrl").

%%Liste des différents feeling

%get_random_message (#jabber{type = connect, size = Size, dest = Dest}) ->
get_random_message (#jabber{type = 'connect'}) ->
    connect();
get_random_message (#jabber{type = 'close'}) ->
    close();
get_random_message (#jabber{type = 'register', id = Id}) when integer(Id), Id > 0 ->
    registration(Id);
get_random_message (#jabber{type = 'register'}) ->
    registration();
get_random_message (#jabber{type = 'presence'}) ->
    presence();
get_random_message (#jabber{type = 'presence:roster', dest=Dest}) ->
    presence(roster, Dest);

get_random_message (#jabber{type = 'authenticate', id = Id}) when integer(Id), Id > 0 ->
    auth(Id);
get_random_message (#jabber{type = 'authenticate'}) ->
    auth();

get_random_message (#jabber{type = 'chat', size = Size,  id = Id, dest = undefined}) ->
    message(Size, ?jabber_domain);
get_random_message (#jabber{type = 'chat', size = Size, id =Id, dest = Dest}) ->
    ?PRINTDEBUG("~w -> ~w ~n", [Id,  Dest], ?DEB),
    message(Dest, Size, ?jabber_domain);



get_random_message (#jabber{type = 'iq:roster:set', dest = Dest}) ->
    requete(roster, "set", Dest);
get_random_message (#jabber{type = 'iq:roster:get', id = Id}) ->
    requete(roster, "get", Id).


%%%%%%%%%%%
%% Connect messages
connect() ->
    list_to_binary(
	  "<stream:stream  id='" ++
	  integer_to_list(ts_msg_server:get_id()) ++
	  "' to='" ++ 
	  ?jabber_domain ++
	  "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>").

%% Close session
close () -> list_to_binary("</stream:stream>").

%% generic Authentication message (auth or register)
auth(Username, Passwd, Type) ->
 list_to_binary(
   "<iq id='" ++integer_to_list(ts_msg_server:get_id()) ++ 
   "' type='set' >" ++
   "<query xmlns='jabber:iq:" ++ Type ++ "'>" ++
   "<username>" ++ Username ++ "</username>" ++ 
   "<resource>tsunami</resource>" ++
   "<password>"++ Passwd ++ "</password></query></iq>").

%% auth message
auth(Username, Passwd) ->
    auth(Username, Passwd, "auth").

%% auth message from a random client
auth() ->
    Id = integer_to_list(ts_user_server:get_id()),
    Name = ?jabber_username ++ Id,
    Passwd = ?jabber_password ++ Id,
    auth(Name, Passwd).


%% auth message from a given client 
auth(Id) ->
    Name = ?jabber_username ++ integer_to_list(Id) ,
    Passwd = ?jabber_password ++ integer_to_list(Id),
    auth(Name, Passwd).


%% register message
registration(Username, Passwd) ->
    auth(Username, Passwd, "register").


%% register message from an random client
registration() ->
    Id = integer_to_list(random:uniform(?jabber_users)),
    Name = ?jabber_username ++ Id,
    Passwd = ?jabber_password ++ Id,
    registration(Name, Passwd).

%% register message from an given client number
registration(Id) when integer(Id)->
    Name = ?jabber_username ++ integer_to_list(Id),
    Passwd = ?jabber_password ++ integer_to_list(Id),
    registration(Name, Passwd).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              <message>
%%
%

%% send message to another random user
message(Size, Service) ->
    message(ts_user_server:get_id(),Size, Service).


%% send message to defined user at the Service (aim, ...)
message(Dest, Size, Service) when integer(Size), Size >= 10 ->
    list_to_binary(
	  "<message id='" ++integer_to_list(ts_msg_server:get_id()) ++ 
	  "' to='" ++ 
	  ?jabber_username ++ integer_to_list(Dest) ++ "@" ++ Service  ++
	  "'><body>" ++ lists:duplicate(Size div 10, "acnkdiejnf") ++ 
	  "</body></message>");

message(Dest, Size, Service) when integer(Size), Size >= 0 ->
    list_to_binary(
	  "<message id='" ++integer_to_list(ts_msg_server:get_id()) ++
	  "' to='"  ++
	  ?jabber_username ++ integer_to_list(Dest) ++ "@" ++ Service  ++ 
	  "'><body>" ++ lists:duplicate(Size, "a") ++
	  "</body></message>").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              <presence>
%%
%

%% presence
presence () -> 
	list_to_binary(
	  "<presence id='" ++integer_to_list(ts_msg_server:get_id()) ++ "' />").


presence(roster, Dest)->
    Name = ?jabber_username ++ integer_to_list(Dest),
    list_to_binary(
	  "<presence id='" ++integer_to_list(ts_msg_server:get_id()) ++ 
	  "' to='" ++ 
	  Name ++ "@"  ++ ?jabber_domain ++
	  "' type='subscribed'/>").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              <iq>

requete(roster, Type, Id)->
    case Type of
		"set"->
			Name = ?jabber_username ++ integer_to_list(Id),
			list_to_binary(
			  "<iq id='" ++integer_to_list(ts_msg_server:get_id()) ++
			  "' type='set'>" ++ "<query xmlns='jabber:iq:roster'><item jid='" ++
			  Name ++ "@"  ++ ?jabber_domain ++
			  "' name='gg1000'/></query></iq>");
		"get"->
			list_to_binary(
			  "<iq id='" ++integer_to_list(ts_msg_server:get_id()) ++ 
			  "' type='get'><query xmlns='jabber:iq:roster'></query></iq>")
    end.

%% In : Intensity : inverse of the mean of inter arrival of messages
%%      N         : number of messages
%% Out: 
get_random_params(Intensity, 1, Size, Type, L) -> 
    L ++ [#message{ ack = no_ack, 
		    thinktime = ?messages_last_time,
		    param = #jabber {size=Size, type=Type}}];

get_random_params(Intensity, N, Size, Type, L)  ->
    get_random_params(Intensity, N-1, Size, Type, 
		      [#message{ ack = no_ack, 
				 thinktime = round(ts_stats:exponential(Intensity)),
				 param = #jabber {size=Size, type=Type}}
		       | L]).

get_random_params(Intensity, N, Size, Type) when integer(N), N >= 0 ->
    get_random_params(Intensity, N, Size, Type, []).

affiche_id([])->
    ok;
affiche_id([H|T])->
    Id = (H#message.param)#jabber.id,
    io:format("~w~n", [Id]),
    affiche_id(T).

