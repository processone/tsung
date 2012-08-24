%%%
%%%  Copyright © Nicolas Niclausse 2007
%%%
%%%  Author : Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%  Created: 17 Mar 2007 by Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
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

-module(ts_test_jabber).
-vc('$Id$ ').
-author('Nicolas.Niclausse@niclux.org').

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_jabber.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->ok.

bidi_subscribeok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribe' to='toto@im.apinc.org' from='tintin@jabber.org'>
  <status>Hi dude.</status>
</presence>"),
    Resp=list_to_binary("<presence to='tintin@jabber.org' type='subscribed'/>"),
    State=#state_rcv{},
    ?assertMatch({Resp,State}, ts_jabber:parse_bidi(Req,State)).

bidi_multisubscribeok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribe' to='toto@im.apinc.org' from='tintin@jabber.org'>  <status>Hi dude.</status></presence><presence type='subscribe' to='toto@im.apinc.org' from='glop@jabber.org'>  <status>Copaing?.</status></presence>"),
    Resp=list_to_binary("<presence to='tintin@jabber.org' type='subscribed'/><presence to='glop@jabber.org' type='subscribed'/>"),
    State=#state_rcv{},
    ?assertMatch({Resp,State}, ts_jabber:parse_bidi(Req,State)).

bidi_multisubscribe_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribe' to='toto@im.apinc.org' from='tintin@jabber.org'>  <status>Hi dude.</status></presence><presence type='subscribed'  from='glop@jabber.org'>  <status>Copaing?.</status></presence>"),
    Resp=list_to_binary("<presence to='tintin@jabber.org' type='subscribed'/>"),
    State=#state_rcv{},
    ?assertMatch({Resp,State}, ts_jabber:parse_bidi(Req,State)).

bidi_subscribe_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribed' from='tintin@jabber.org'>
  <status>Hi dude.</status>
</presence>"),
    State=#state_rcv{},
    ?assertMatch({nodata,State}, ts_jabber:parse_bidi(Req,State)).

bidi_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence from='tintin@jabber.org'><status>Alive.</status></presence>"),
    State=#state_rcv{},
    ?assertMatch({nodata,State}, ts_jabber:parse_bidi(Req,State)).

auth_sasl_test()->
    myset_env(),
    Res = << "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN' >AGp1bGlldAByMG0zMG15cjBtMzA=</auth>" >>,
    ?assertMatch(Res, ts_jabber_common:auth_sasl("juliet","r0m30myr0m30","PLAIN")).

add_dynparams_test()->
    ts_user_server:start(),
    ts_user_server:reset(100),
    ts_msg_server:start(),
    Req = #jabber{id=0,prefix="foo",username="foo",passwd="bar",type='connect',domain={domain,"localdomain"}},
    {_,Session} = ts_jabber:get_message(Req,#state_rcv{session=#jabber_session{}}),
    ?assertEqual(Req#jabber{id=1,username="foo1",passwd="bar1",user_server=default,domain="localdomain"}, ts_jabber:add_dynparams(true,{[],Session},Req,"localhost")).

add_dynparams2_test()->
    Req = #jabber{id=0,prefix="foo",username="foo",passwd="bar",type='connect', domain={domain,"localdomain"}},
    {_,Session} = ts_jabber:get_message(Req,#state_rcv{session=#jabber_session{}}),
    ?assertEqual(Req#jabber{id=2,username="foo2",passwd="bar2",user_server=default,domain="localdomain"}, ts_jabber:add_dynparams(true,{[],Session},Req,"localhost")).

get_message_test()->
    Req = #jabber{id=0,prefix="foo",username="foo",type='auth_set_plain',passwd="bar",domain={domain,"localdomain"},resource="tsung"},
    RepOK = <<"<iq id='3' type='set' ><query xmlns='jabber:iq:auth'><username>foo3</username><resource>tsung</resource><password>bar3</password></query></iq>" >>,
    {Rep,_}=ts_jabber:get_message(Req,#state_rcv{session=#jabber_session{}}),
    ?assertEqual(RepOK,Rep ).

get_message2_test()->
    Req = #jabber{id=user_defined,username="foo",type='auth_set_plain',passwd="bar",domain={domain,"localdomain"},resource="tsung"},
    RepOK = <<"<iq id='4' type='set' ><query xmlns='jabber:iq:auth'><username>foo</username><resource>tsung</resource><password>bar</password></query></iq>" >>,
    {Rep,_} = ts_jabber:get_message(Req,#state_rcv{session=#jabber_session{}}),
    ?assertEqual(RepOK,Rep).

pubsub_unsubscribe_test()->
    ts_user_server:reset(1),
    Req = #jabber{id=0,prefix="foo",username="foo",type='pubsub:unsubscribe',passwd="bar",domain={domain,"localdomain"},dest=random, node="node", pubsub_service="mypubsub", subid="myid",resource="tsung"},
    RepOK= << "<iq to='mypubsub' type='set' id='5'><pubsub xmlns='http://jabber.org/protocol/pubsub'><unsubscribe node='/home/localdomain/foo2/node' jid='foo1@localdomain' subid='myid'/></pubsub></iq>" >>,
    {Rep,_}=ts_jabber:get_message(Req,#state_rcv{session=#jabber_session{}}),
    ?assertEqual(RepOK,Rep ).

connect_legacy_test()->
    ts_user_server:reset(1),
    Req = #jabber{id=0,prefix="foo",username="foo",type='connect',passwd="bar",domain={domain,"localdomain"},resource="tsung", version="legacy"},
    RepOK= <<"<?xml version='1.0'?><stream:stream  id='6' to='localdomain' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>" >>,
    {Rep,_} = ts_jabber:get_message(Req,#state_rcv{session=#jabber_session{}}),
    ?assertEqual(RepOK,Rep).

connect_xmpp_test()->
    ts_user_server:reset(1),
    Req = #jabber{id=0,prefix="foo",username="foo",type='connect',passwd="bar",domain={domain,"localdomain"},resource="tsung", version="1.0"},
    RepOK= <<"<?xml version='1.0'?><stream:stream  id='7' to='localdomain' xmlns='jabber:client' version='1.0' xmlns:stream='http://etherx.jabber.org/streams'>" >>,
    {Rep,_} = ts_jabber:get_message(Req,#state_rcv{session=#jabber_session{}}),
    ?assertEqual(RepOK,Rep).


pubsub_subscribe_test()->
    ts_user_server:reset(1),
    Req = #jabber{id=0,prefix="foo",dest="foo2",username="foo",type='pubsub:subscribe',passwd="bar",domain={domain,"localdomain"},node="node", pubsub_service="mypubsub", resource="tsung"},
    RepOK= << "<iq to='mypubsub' type='set' id='8'><pubsub xmlns='http://jabber.org/protocol/pubsub'><subscribe node='/home/localdomain/foo2/node' jid='foo1@localdomain'/></pubsub></iq>" >>,
    {Rep,_}=ts_jabber:get_message(Req,#state_rcv{session=#jabber_session{}}),
    ?assertEqual(RepOK,Rep ).

myset_env()->
    myset_env(0).
myset_env(Val)->
    application:set_env(stdlib,debug_level,Val).
