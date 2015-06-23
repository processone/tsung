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

-include("ts_macros.hrl").
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
    ?assertMatch({Resp,State,think}, ts_jabber:parse_bidi(Req,State)).

bidi_multisubscribeok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribe' to='toto@im.apinc.org' from='tintin@jabber.org'>  <status>Hi dude.</status></presence><presence type='subscribe' to='toto@im.apinc.org' from='glop@jabber.org'>  <status>Copaing?.</status></presence>"),
    Resp=list_to_binary("<presence to='tintin@jabber.org' type='subscribed'/><presence to='glop@jabber.org' type='subscribed'/>"),
    State=#state_rcv{},
    ?assertMatch({Resp,State,think}, ts_jabber:parse_bidi(Req,State)).

bidi_multisubscribe_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribe' to='toto@im.apinc.org' from='tintin@jabber.org'>  <status>Hi dude.</status></presence><presence type='subscribed'  from='glop@jabber.org'>  <status>Copaing?.</status></presence>"),
    Resp=list_to_binary("<presence to='tintin@jabber.org' type='subscribed'/>"),
    State=#state_rcv{},
    ?assertMatch({Resp,State,think}, ts_jabber:parse_bidi(Req,State)).

bidi_subscribe_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribed' from='tintin@jabber.org'>
  <status>Hi dude.</status>
</presence>"),
    State=#state_rcv{},
    ?assertMatch({nodata,State,think}, ts_jabber:parse_bidi(Req,State)).

bidi_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence from='tintin@jabber.org'><status>Alive.</status></presence>"),
    State=#state_rcv{},
    ?assertMatch({nodata,State,think}, ts_jabber:parse_bidi(Req,State)).

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
    RepOK= << "<iq to='mypubsub' type='set' id='5'><pubsub xmlns='http://jabber.org/protocol/pubsub'><unsubscribe node='/home/localdomain/foo1/node' jid='foo1@localdomain' subid='myid'/></pubsub></iq>" >>,
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

get_online_test()->
    ts_user_server:reset(100),
    Id=ts_user_server:get_idle(),
    IdOther = ts_user_server:get_idle(),
    RealId  = ts_jabber_common:set_id(IdOther,"tsung","tsung"),
    ts_user_server:add_to_online(default,RealId),
    {ok,Offline} = ts_user_server:get_offline(),
    {ok,Online}  = ts_user_server:get_online(Id),
    ?assertEqual({1,3,2},{Id,Offline,Online} ).

get_online_user_test()->
    Server="myserver",
    ts_user_server_sup:start_user_server(list_to_atom("us_" ++Server)),
    MyServer = global:whereis_name(list_to_atom("us_"++Server)),
    ts_user_server:reset(MyServer,100),
    Id=ts_user_server:get_idle(MyServer),
    IdOther = ts_user_server:get_idle(MyServer),
    RealId  = ts_jabber_common:set_id(IdOther,"tsung","tsung"),
    ts_user_server:add_to_online(MyServer,RealId),
    {ok,Offline} = ts_user_server:get_offline(MyServer),
    {ok,Online}  = ts_user_server:get_online(MyServer, Id),
    ?assertEqual({1,3,2},{Id,Offline,Online} ).

get_online_user_defined_test()->
    ts_user_server:reset(0),
    ts_msg_server:stop(),
    ts_msg_server:start(),
    User1 =  "tsung1",
    User2 =  "tsung2",
    User3 =  "tsung3",
    Pwd   =  "sesame",
    ts_user_server:add_to_connected({User1,Pwd}),
    ts_user_server:add_to_online(default, ts_jabber_common:set_id(user_defined,User1, Pwd) ),

    ts_user_server:add_to_connected({User2,Pwd}),
    ts_user_server:add_to_online(default, ts_jabber_common:set_id(user_defined,User2, Pwd) ),

    ts_user_server:add_to_connected({User3,Pwd}),
    ts_user_server:add_to_online(default, ts_jabber_common:set_id(user_defined,User3, Pwd) ),
    ts_user_server:remove_connected(default, ts_jabber_common:set_id(user_defined,User3, Pwd) ),

    {ok,Offline}=ts_user_server:get_offline(),

    Msg = ts_jabber_common:get_message(#jabber{type = 'presence:directed', id=user_defined,username=User1,passwd=Pwd,prefix="prefix",
                                               show = "foo", status="mystatus",user_server=default, domain="domain.org"}),
    Res = "<presence id='1' to='tsung2@domain.org'><show>foo</show><status>mystatus</status></presence>",
    ?assertEqual(Res, binary_to_list(Msg) ).

get_offline_user_defined_test()->
    ts_user_server:reset(0),
    User1 =  "tsung1",
    User3 =  "tsung3",
    Pwd   =  "sesame",
    ts_user_server:add_to_connected({User1,Pwd}),
    ts_user_server:add_to_online(default, ts_jabber_common:set_id(user_defined,User1, Pwd) ),

    ts_user_server:add_to_connected({User3,Pwd}),
    ts_user_server:add_to_online(default, ts_jabber_common:set_id(user_defined,User3, Pwd) ),
    ts_user_server:remove_connected(default, ts_jabber_common:set_id(user_defined,User3, Pwd) ),

    Msg = ts_jabber_common:get_message(#jabber{type = 'chat', prefix="prefix", data="hello", dest = offline, user_server=default, domain="domain.org"}),
    Res = "<message id='2' to='tsung3@domain.org' type='chat'><body>hello</body></message>",
    ?assertEqual(Res, binary_to_list(Msg) ).

get_unique_user_defined_test()-> % this test must be runned just after get_offline_user_defined_test
    Msg = ts_jabber_common:get_message(#jabber{type = 'chat', prefix="prefix", data="hello", dest = unique, user_server=default, domain="domain.org"}),
    Res = "<message id='3' to='tsung1@domain.org' type='chat'><body>hello</body></message>",
    ?assertEqual(Res, binary_to_list(Msg) ).



get_unique_test()->
    ts_user_server:reset(2),
    Id=ts_user_server:get_idle(),
    Msg = ts_jabber_common:get_message(#jabber{type = 'chat', prefix="prefix", data="hello", dest = unique, user_server=default, domain="domain.org"}),
    Res = "<message id='4' to='prefix1@domain.org' type='chat'><body>hello</body></message>",
    ?assertEqual(Res, binary_to_list(Msg) ).

get_random_test()->
    ts_user_server:reset(1),
    Msg = ts_jabber_common:get_message(#jabber{type = 'chat', prefix="prefix", data="hello", dest = random, user_server=default, domain="domain.org"}),
    Res = "<message id='5' to='prefix1@domain.org' type='chat'><body>hello</body></message>",
    ?assertEqual(Res, binary_to_list(Msg) ).

get_random_user_defined_test()->
    ts_user_server:reset(0),
    Id = xmpp,
    ts_user_server:set_random_fileid(Id),
    ts_file_server:start(),
    ts_file_server:read([{default,"./src/test/test_file_server.csv"},
                         {Id,"./src/test/test_file_server2.csv"} ]),

    Msg = ts_jabber_common:get_message(#jabber{type = 'chat', prefix="prefix", data="hello", dest = random, user_server=default, domain="domain.org"}),
    Res = "<message id='6' to='user1@domain.org' type='chat'><body>hello</body></message>",
    ?assertEqual(Res, binary_to_list(Msg) ).

get_offline_user_defined_offline_test()->
    Id = xmpp,
    ts_user_server:set_offline_fileid(Id),
    ts_user_server:reset(0),
    User1 =  "tsung1",
    Pwd   =  "sesame",
    ts_user_server:add_to_connected({User1,Pwd}),
    ts_user_server:add_to_online(default, ts_jabber_common:set_id(user_defined,User1, Pwd) ),

    Msg = ts_jabber_common:get_message(#jabber{type = 'chat', prefix="prefix", data="hello", dest = offline, user_server=default, domain="domain.org"}),
    Res = "<message id='7' to='user1@domain.org' type='chat'><body>hello</body></message>",
    ?assertEqual(Res, binary_to_list(Msg) ).

get_offline_user_defined_no_offline_test()->
    ts_user_server:reset(0),
    User1 =  "user1",
    Pwd   =  "sesame",
    ts_user_server:add_to_connected({User1,Pwd}),
    ts_user_server:add_to_online(default, ts_jabber_common:set_id(user_defined,User1, Pwd) ),

    Msg = ts_jabber_common:get_message(#jabber{type = 'chat', prefix="prefix", data="hello", dest = offline, user_server=default, domain="domain.org"}),
    %% Res = "<message id='8' to='user1@domain.org' type='chat'><body>hello</body></message>",
    Res = "",
    ts_user_server:set_offline_fileid(undefined),
    ?assertEqual(Res, binary_to_list(Msg) ).

myset_env()->
    myset_env(0).
myset_env(Val)->
    application:set_env(stdlib,debug_level,Val).
