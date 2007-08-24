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

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

-module(ts_jabber_common).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([ get_random_params/4,
          get_message/1
         ]).

-include("ts_profile.hrl").
-include("ts_jabber.hrl").

%%----------------------------------------------------------------------
%% Func: get_message/1
%% Args: #jabber record
%% Returns: binary
%% Purpose: Build a message/request from a #jabber record
%%----------------------------------------------------------------------
get_message(Jabber=#jabber{type = 'connect'}) ->
    connect(Jabber);
get_message(#jabber{type = 'close', id=Id}) ->
    ts_user_server:remove_connected(Id),
    close();
get_message(#jabber{type = 'presence'}) ->
    presence();
get_message(#jabber{type = 'presence:initial', id=Id}) ->
    ts_user_server:add_to_online(Id),
    presence();
get_message(#jabber{type = 'presence:final', id=Id}) ->
    ts_user_server:remove_from_online(Id),
    presence(unavailable);
get_message(#jabber{type = 'presence:broadcast', show=Show, status=Status}) ->
    presence(broadcast, Show, Status);
get_message(Jabber=#jabber{type = 'presence:directed', id=Id, show=Show, status=Status}) ->
    case ts_user_server:get_online(Id) of
        {ok, Dest} ->
            presence(directed, Dest, Jabber, Show, Status);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;

get_message(Jabber=#jabber{id=Id}) when is_integer(Id)->
    get_message(Jabber#jabber{id=id_to_string(Id)});
get_message(Jabber=#jabber{dest=previous}) ->
    Dest = get(previous),
    get_message(Jabber#jabber{dest=Dest});
get_message(Jabber=#jabber{type = 'presence:roster'}) ->
    presence(roster, Jabber);
get_message(#jabber{type = 'presence:subscribe'}) -> %% must be called AFTER iq:roster:add
    RosterJid = get(rosterjid),
    presence(subscribe, RosterJid);
get_message(Jabber=#jabber{type = 'chat', id=Id, dest=online, domain=Domain})->
    case ts_user_server:get_online(Id) of
        {ok, Dest} ->
            message(Dest, Jabber, Domain);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;

get_message(Jabber=#jabber{type = 'chat', domain = Domain, dest=offline}) ->
    case ts_user_server:get_offline() of
        {ok, Dest} ->
            message(Dest, Jabber, Domain);
        {error, no_offline} ->
            ts_mon:add({ count, error_no_offline }),
            << >>
    end;
get_message(Jabber=#jabber{type = 'chat', dest=random, domain=Domain}) ->
    Dest = ts_user_server:get_id(),
    message(Dest, Jabber, Domain);
get_message(Jabber=#jabber{type = 'chat', dest=unique, domain=Domain})->
    {Dest, _} = ts_user_server:get_first(),
    message(Dest, Jabber, Domain);
get_message(Jabber=#jabber{type = 'chat', id=_Id, dest = Dest, domain=Domain}) ->
    ?DebugF("~w -> ~w ~n", [_Id,  Dest]),
    message(Dest, Jabber, Domain);
get_message(#jabber{type = 'iq:roster:add', id=Id, dest = online,username=User,domain=Domain}) ->
    case ts_user_server:get_online(Id) of
        {ok, Dest} ->
            request(roster_add, User, Domain, Dest);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;
get_message(#jabber{type = 'iq:roster:add',dest = offline,username=User,domain=Domain})->
    case ts_user_server:get_offline() of
        {ok, Dest} ->
            request(roster_add, User, Domain, Dest);
        {error, no_offline} ->
            ts_mon:add({ count, error_no_offline }),
            << >>
    end;
get_message(#jabber{type = 'iq:roster:rename'})-> %% must be called AFTER iq:roster:add
        RosterJid = get(rosterjid),
        request(roster_rename, RosterJid);
get_message(#jabber{type = 'iq:roster:remove'})-> %% must be called AFTER iq:roster:add
        RosterJid = get(rosterjid),
        request(roster_remove, RosterJid);
get_message(#jabber{type = 'iq:roster:get', id = Id,username=User,domain=Domain}) ->
    request(roster_get, User, Domain, Id);

get_message(Jabber=#jabber{type = 'raw'}) ->
    raw(Jabber);

%% -- Pubsub benchmark support --
%% For node creation, data contains the pubsub nodename (relative to user
%% hierarchy or absolute, optional)
get_message(#jabber{type = 'pubsub:create', id=Id, username=User,
                    data=Data, domain = Domain}) ->
    Username = username(User,Id),
    create_pubsub_node(Domain, Username, Data);
%% For node subscription, data contain the pubsub nodename (relative to user
%% hierarchy or absolute)
get_message(#jabber{type = 'pubsub:subscribe', id=Id, username=User,
                    dest=online, data=Data, domain = Domain}) ->
    case ts_user_server:get_online(Id) of
        {ok, Dest} ->
            UserFrom = username(User,Id),
            UserTo = username(User, id_to_string(Dest)),
            subscribe_pubsub_node(Domain, UserFrom, UserTo, Data);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;
get_message(#jabber{type = 'pubsub:subscribe', id=Id, username=User,
                    dest=offline, data=Data, domain = Domain}) ->
    case ts_user_server:get_offline() of
        {ok, Dest} ->
            UserFrom = username(User,Id),
            UserTo = username(User,id_to_string(Dest)),
            subscribe_pubsub_node(Domain, UserFrom, UserTo, Data);
        {error, no_offline} ->
            ts_mon:add({ count, error_no_offline }),
            << >>
    end;
%% For node publication, data contain the pubsub nodename (relative to user
%% hierarchy or absolute)
get_message(#jabber{type = 'pubsub:publish', size=Size, id=Id,
                    username=User, dest=online, data=Data,
                    domain = Domain}) ->
    case ts_user_server:get_online(Id) of
        {ok, _Dest} -> %% FIXME: Dest not used ?!
            Username = username(User,Id),
            publish_pubsub_node(Domain, Username, Data, Size);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;
get_message(#jabber{type = 'pubsub:publish', size=Size, id=Id,
                    username=User, dest=offline, data=Data,
                    domain = Domain}) ->
    case ts_user_server:get_offline() of
        {ok, _Dest} -> %% FIXME: Dest not used ?!
            Username = username(User,Id),
            publish_pubsub_node(Domain, Username, Data, Size);
        {error, no_offline} ->
            ts_mon:add({ count, error_no_offline }),
            << >>
    end;

get_message(Jabber=#jabber{username=Name, passwd=Passwd, id=Id}) ->
    FullName = username(Name, Id),
    FullPasswd = password(Passwd,Id),
    get_message2(Jabber#jabber{username=FullName,passwd=FullPasswd}).


%%----------------------------------------------------------------------
%% Func: get_message2/1
%%----------------------------------------------------------------------
get_message2(Jabber=#jabber{type = 'register'}) ->
    registration(Jabber);
get_message2(Jabber=#jabber{type = 'auth_get'}) ->
    auth_get(Jabber);
get_message2(Jabber=#jabber{type = 'auth_set_plain'}) ->
    auth_set_plain(Jabber);
get_message2(Jabber=#jabber{type = 'auth_set_digest', sid=Sid}) ->
    auth_set_digest(Jabber,Sid);
get_message2(Jabber=#jabber{type = 'auth_set_sip', domain=Realm, nonce=Nonce}) ->
    auth_set_sip(Jabber,Nonce,Realm);
get_message2(#jabber{type = Type}) ->
    ?LOGF("Unknown message type: ~p~n", [Type], ?ERR),
    {error, unknown_message}.



%%----------------------------------------------------------------------
%% Func: connect/1
%%----------------------------------------------------------------------
connect(#jabber{domain=Domain}) ->
    list_to_binary([
      "<stream:stream  id='",
      ts_msg_server:get_id(list),
      "' to='",
      Domain,
      "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>"]).

%%----------------------------------------------------------------------
%% Func: close/0
%% Purpose: close jabber session
%%----------------------------------------------------------------------
close () -> list_to_binary("</stream:stream>").

%%----------------------------------------------------------------------
%% Func: auth_get/1
%%----------------------------------------------------------------------
auth_get(#jabber{username=Name,passwd=Passwd})->
    auth_get(Name, Passwd, "auth").

%%----------------------------------------------------------------------
%% Func: auth_get/3
%%----------------------------------------------------------------------
auth_get(Username, _Passwd, Type) ->
 list_to_binary([
   "<iq id='", ts_msg_server:get_id(list),
   "' type='get' >",
   "<query xmlns='jabber:iq:", Type, "'>",
   "<username>", Username, "</username></query></iq>"]).

%%----------------------------------------------------------------------
%% Func: auth_set_plain/1
%%----------------------------------------------------------------------
auth_set_plain(#jabber{username=Name,passwd=Passwd})->
    auth_set_plain(Name, Passwd, "auth").


%%----------------------------------------------------------------------
%% Func: auth_set_plain/3
%%----------------------------------------------------------------------
auth_set_plain(Username, Passwd, Type) ->
 list_to_binary([
   "<iq id='", ts_msg_server:get_id(list),
   "' type='set' >",
   "<query xmlns='jabber:iq:", Type, "'>",
   "<username>", Username, "</username>",
   "<resource>tsung</resource>",
   "<password>", Passwd, "</password></query></iq>"]).


%%----------------------------------------------------------------------
%% Func: auth_set_digest/2
%%----------------------------------------------------------------------
auth_set_digest(#jabber{username=Name,passwd=Passwd}, Sid)->
        auth_set_digest(Name, Passwd, "auth", Sid).


%%----------------------------------------------------------------------
%% Func: auth_set_digest/4
%%----------------------------------------------------------------------
auth_set_digest(Username, Passwd, Type, Sid) ->
 {Digest} = ts_digest:digest(Sid, Passwd),
 list_to_binary([
   "<iq id='", ts_msg_server:get_id(list),
   "' type='set' >",
   "<query xmlns='jabber:iq:", Type, "'>",
   "<username>", Username, "</username>",
   "<resource>tsung</resource>",
   "<digest>", Digest, "</digest></query></iq>"]).


%%----------------------------------------------------------------------
%% Func: auth_set_sip/3
%%----------------------------------------------------------------------
auth_set_sip(#jabber{username=Name,passwd=Passwd,domain=Domain}, Nonce, Realm)->
        auth_set_sip(Name, Passwd, Domain, "auth", Nonce, Realm).

%%----------------------------------------------------------------------
%% Func: auth_set_sip/6
%%----------------------------------------------------------------------
auth_set_sip(Username, Passwd, Domain, Type, Nonce, Realm) ->
 Jid = Username ++ "@" ++ Realm,
 {SipDigest,Integrity} = ts_digest:sip_digest(Nonce, Jid, Realm, Passwd),
 list_to_binary([
   "<iq id='", ts_msg_server:get_id(list),
   "' type='set' >",
   "<query xmlns='jabber:iq:", Type, "'>",
        "<username>", Jid, "</username>",
        "<resource>tsung</resource>",
        "<x xmlns='xmpp:assert' version='1.0'>",
                "<ContextInfo><ServiceValue><Realm>", Domain,
                "</Realm></ServiceValue></ContextInfo>",
                "<TokenInfo><SubjectValue>",
                        "<Username>", Jid, "</Username>",
                        "<Password type='sip-digest' encoding='hex'>", SipDigest,
                                "</Password>",
                        "<Nonce encoding='hex'>", Nonce, "</Nonce>",
                        "<Integrity encoding='hex'>", Integrity, "</Integrity>",
        "</SubjectValue></TokenInfo></x></query></iq>"]).



%%----------------------------------------------------------------------
%% Func: registration/1
%% Purpose: register message
%%----------------------------------------------------------------------
registration(#jabber{username=Name,passwd=Passwd})->
    auth_set_plain(Name, Passwd, "register").

%%----------------------------------------------------------------------
%% Func: message/3
%% Purpose: send message to defined user at the Service (aim, ...)
%%----------------------------------------------------------------------
message(Dest, Jabber, Service) when is_integer(Dest) ->
    message(id_to_string(Dest),Jabber, Service);
message(Dest, #jabber{size=Size,data=undefined, username=User}, Service) when is_integer(Size) ->
    put(previous, Dest),
    Username = username(User,Dest),
    list_to_binary([
                    "<message id='",ts_msg_server:get_id(list), "' to='",
                    Username, "@", Service,
                    "'><body>",garbage(Size), "</body></message>"]);
message(Dest, #jabber{data=Data, username=User}, Service) when is_list(Data) ->
    put(previous, Dest),
    Username = username(User,Dest),
    list_to_binary([
                    "<message id='",ts_msg_server:get_id(list), "' to='",
                    Username, "@", Service,
                    "'><body>",Data, "</body></message>"]).

%%----------------------------------------------------------------------
%% Func:    garbage/1
%% Purpose: generate list of given size. Implemented by duplicating list
%% of length 10 to be faster
%%----------------------------------------------------------------------
garbage(Size) when Size >= 10 ->
    Msg= lists:duplicate(Size div 10,"0123456789"),
    case Size rem 10 of
        0->
            Msg;
        Rest ->
            lists:append(Msg,garbage(Rest))
    end;
garbage(Size)->
    lists:duplicate(Size rem 10,"a").


%%----------------------------------------------------------------------
%% Func: presence/0
%%----------------------------------------------------------------------
presence() ->
    list_to_binary([ "<presence id='",ts_msg_server:get_id(list),"' />"]).

%%----------------------------------------------------------------------
%% Func: presence/1
%%----------------------------------------------------------------------
presence(unavailable)->
    list_to_binary([ "<presence type='unavailable'/>"]).

%%----------------------------------------------------------------------
%% Func: presence/2
%%----------------------------------------------------------------------
presence(Type, Jabber=#jabber{dest=Dest}) when is_integer(Dest)->
    presence(Type, Jabber#jabber{dest=integer_to_list(Dest)}) ;
presence(roster, Jabber)->
    presence(subscribed, Jabber);
presence(subscribe, RosterJid)->
     list_to_binary([
           "<presence id='",ts_msg_server:get_id(list),
           "' to='", RosterJid,
           "' type='subscribe'/>"]);
presence(Type, Jabber) when is_atom(Type)->
    presence(atom_to_list(Type), Jabber);
presence(Type, #jabber{dest=Dest, domain=Domain, username=UserName})->
    DestName = username(UserName, Dest),
    list_to_binary([
      "<presence id='",ts_msg_server:get_id(list),
      "' to='", DestName, "@" , Domain,
      "' type='",Type,"'/>"]).

%%----------------------------------------------------------------------
%% Func: presence/3
%%----------------------------------------------------------------------
presence(broadcast, Show, Status) ->
    list_to_binary([ "<presence id='",ts_msg_server:get_id(list),"'>",
        "<show>", Show, "</show><status>", Status, "</status></presence>"]).

%%----------------------------------------------------------------------
%% Func: presence/4
%%----------------------------------------------------------------------
presence(directed, Dest, Jabber, Show, Status) when is_integer(Dest) ->
    presence(directed, integer_to_list(Dest), Jabber, Show, Status);
presence(directed, Dest, #jabber{username=UserName,domain=Domain}, Show, Status) ->
    DestName = username(UserName,Dest),
    list_to_binary([
          "<presence id='",ts_msg_server:get_id(list),
          "' to='", DestName, "@" , Domain , "'>",
          "<show>", Show, "</show><status>", Status, "</status></presence>"]).

%%----------------------------------------------------------------------
%% Func: request/2
%%----------------------------------------------------------------------
request(roster_rename, RosterJid) ->
        list_to_binary([
                "<iq id='" ,ts_msg_server:get_id(list),
                "' type='set'><query xmlns='jabber:iq:roster'><item jid='"
                ,RosterJid,
                "' name='Tsung Testuser'><group>Tsung Group</group></item></query></iq>"]);
request(roster_remove, RosterJid) ->
        list_to_binary([
                "<iq id='" ,ts_msg_server:get_id(list),
                "' type='set'><query xmlns='jabber:iq:roster'><item jid='"
                ,RosterJid,
                "' subscription='remove'/></query></iq>"]).
%%----------------------------------------------------------------------
%% Func: request/4
%%----------------------------------------------------------------------
request(roster_add, UserName, Domain, Id) when is_integer(Id)->
    request(roster_add, UserName, Domain, integer_to_list(Id));
request(roster_add, UserName, Domain, Id)->
        Name = username(UserName,Id),
        RosterJid = Name ++ "@" ++ Domain,
        _ = put(rosterjid,RosterJid),
        list_to_binary([
                "<iq id='" ,ts_msg_server:get_id(list),
                "' type='set'>","<query xmlns='jabber:iq:roster'><item jid='",
                RosterJid,
                "' name='",RosterJid,"'><group>Tsung Group</group></item></query></iq>"]);
request(roster_get, _UserName, _Domain, _Id)->
    list_to_binary([
      "<iq id='" ,ts_msg_server:get_id(list),
      "' type='get'><query xmlns='jabber:iq:roster'></query></iq>"]).

%%----------------------------------------------------------------------
%% Func: get_random_params/5
%% Args: Intensity (inverse of the mean of inter arrival of messages)
%%       N         : number of messages
%%----------------------------------------------------------------------
get_random_params(_Intensity, 1, Size, Type, L) ->
    L ++ [#ts_request{ ack = no_ack,
            thinktime = ?config(messages_last_time),
            param = #jabber {size=Size, type=Type}}];

get_random_params(Intensity, N, Size, Type, L)  ->
    get_random_params(Intensity, N-1, Size, Type,
              [#ts_request{ack = no_ack,
                           thinktime = round(ts_stats:exponential(Intensity)),
                           param = #jabber {size=Size, type=Type}}
               | L]).

get_random_params(Intensity, N, Size, Type) when is_integer(N), N >= 0 ->
    get_random_params(Intensity, N, Size, Type, []).


%%%----------------------------------------------------------------------
%%% Func: raw/1
%%%----------------------------------------------------------------------
raw(#jabber{data=undefined}) ->
    << >>;
raw(#jabber{data=Data}) when is_list(Data) ->
    list_to_binary(Data).

%%%----------------------------------------------------------------------
%%% Func: create_pubsub_node/3
%%% Create a pubsub node: Generate XML packet
%%% If node name is undefined (data attribute), we create a pubsub instant
%%% node.
%%% Nodenames are relative to the User pubsub hierarchy (ejabberd); they are
%%% absolute with leading slash.
%%%----------------------------------------------------------------------
create_pubsub_node(Domain, Username, Node) ->
    Result = list_to_binary(["<iq to='pubsub.", Domain, "' type='set' id='",
            ts_msg_server:get_id(list),"'>"
            "<pubsub xmlns='http://jabber.org/protocol/pubsub'>"
            "<create", pubsub_node_attr(Node, Domain, Username),
            "/></pubsub></iq>"]),
    Result.
%% Generate pubsub node attribute
pubsub_node_attr(undefined, _Domain, _Username) -> " ";
pubsub_node_attr([$/|AbsNode], _Domain, _Username) ->
    [" node='/", AbsNode,"'"];
pubsub_node_attr(Node, Domain, Username) ->
    [" node='/home/", Domain, "/", Username, "/", Node,"'"].

%%%----------------------------------------------------------------------
%%% Func: subscribe_pubsub_node/4
%%% Subscribe to a pubsub node: Generate XML packet
%%% If node name is undefined (data attribute), we subscribe to target user
%%% root node
%%% Nodenames are relative to the User pubsub hierarchy (ejabberd); they are
%%% absolute with leading slash.
%%%----------------------------------------------------------------------
subscribe_pubsub_node(Domain, UserFrom, UserTo, undefined) ->
    subscribe_pubsub_node(Domain, UserFrom, UserTo, "");
subscribe_pubsub_node(Domain, UserFrom, UserTo, Node) ->
    list_to_binary(["<iq to='pubsub.", Domain, "' type='set' id='",
            ts_msg_server:get_id(list),"'>"
            "<pubsub xmlns='http://jabber.org/protocol/pubsub'>"
            "<subscribe", pubsub_node_attr(Node, Domain, UserTo),
            " jid='", UserFrom, "@", Domain, "'/>"
            "</pubsub></iq>"]).

%%%----------------------------------------------------------------------
%%% Func: publish_pubsub_node/4
%%% Publish an item to a pubsub node
%%% Nodenames are relative to the User pubsub hierarchy (ejabberd); they are
%%% absolute with leading slash.
%%%----------------------------------------------------------------------
publish_pubsub_node(Domain, Username, undefined, Size) ->
    publish_pubsub_node(Domain, Username, "", Size);
publish_pubsub_node(Domain, Username, Node, Size) ->
    Result = list_to_binary(["<iq to='pubsub.", Domain, "' type='set' id='",
            ts_msg_server:get_id(list),"'>"
            "<pubsub xmlns='http://jabber.org/protocol/pubsub'>"
            "<publish", pubsub_node_attr(Node, Domain, Username),">"
            "<item>", garbage(Size),"</item></publish>"
            "</pubsub></iq>"]),
    Result.

%%%----------------------------------------------------------------------
%%% Func: username/2
%%% Generate the username given a prefix and id
%%%----------------------------------------------------------------------
username(Prefix, Id) ->
    Prefix ++ Id.
%%% Convert Id to string
%%% Change this if you want to have padding
id_to_string(Id) ->
    integer_to_list(Id).

%%%----------------------------------------------------------------------
%%% Func: password/1
%%% Generate password for a given username
%%%----------------------------------------------------------------------
password(Prefix,Id) ->
    Prefix ++ Id.
