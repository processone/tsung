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

choose_id_user_test()->
    ?assertEqual({user_defined,"user","pwd"}, ts_jabber:choose_or_cache_user_id(user_defined,"user","pwd")).
choose_id_user1_test()->
    ts_jabber:choose_or_cache_user_id(user_defined,"user","pwd"),
    ?assertEqual({user_defined,"user","pwd"}, ts_jabber:choose_or_cache_user_id(0,"user","pwd")).
choose_id2_test()->
    erase(xmpp_user_id),
    ts_user_server:start(),
    ts_user_server:reset(100),
    ?assertEqual({1,"user","pwd"}, ts_jabber:choose_or_cache_user_id(0,"user","pwd")).
choose_id3_test()->
    erase(xmpp_user_id),
    ts_jabber:choose_or_cache_user_id(0,"user","pwd"),
    ?assertEqual({2,"user","pwd"}, ts_jabber:choose_or_cache_user_id(0,"user","pwd")).

add_dynparams_test()->
    erase(xmpp_user_id),
    Session = #jabber{id=0,username="foo",passwd="bar",domain={domain,"localdomain"}},
    ?assertEqual(Session#jabber{id=3,user_server=default,domain="localdomain"}, ts_jabber:add_dynparams(true,[],Session,"localhost")).

add_dynparams2_test()->
    Session = #jabber{id=0,username="foo",passwd="bar",domain={domain,"localdomain"}},
    ?assertEqual(Session#jabber{id=3,user_server=default,domain="localdomain"}, ts_jabber:add_dynparams(true,[],Session,"localhost")).

get_message_test()->
    erase(xmpp_user_id),
    ts_msg_server:start(),
    Session = #jabber{id=0,username="foo",type='auth_set_plain',passwd="bar",domain={domain,"localdomain"}},
    Req=ts_jabber:add_dynparams(false,[],Session,"localhost"),
    RepOK={<<"<iq id='1' type='set' ><query xmlns='jabber:iq:auth'><username>foo4</username><resource>tsung</resource><password>bar4</password></query></iq>" >>,undefined},
    Rep=ts_jabber:get_message(Req,#state_rcv{}),
    ?assertEqual(RepOK,Rep ).

get_message2_test()->
    erase(xmpp_user_id),
    Session = #jabber{id=user_defined,username="foo",type='auth_set_plain',passwd="bar",domain={domain,"localdomain"}},
    Req=ts_jabber:add_dynparams(false,[],Session,"localhost"),
    RepOK={<<"<iq id='2' type='set' ><query xmlns='jabber:iq:auth'><username>foo</username><resource>tsung</resource><password>bar</password></query></iq>" >>,undefined},
    Rep=ts_jabber:get_message(Req,#state_rcv{}),
    ?assertEqual(RepOK,Rep ).


choose_id_limit_test()->
    ts_user_server:reset(3),
    erase(xmpp_user_id),
    ts_jabber:choose_or_cache_user_id(0,"user","pwd"),
    erase(xmpp_user_id),
    ts_jabber:choose_or_cache_user_id(0,"user","pwd"),
    erase(xmpp_user_id),
    ts_jabber:choose_or_cache_user_id(0,"user","pwd"),
    erase(xmpp_user_id),
    ?assertExit(no_free_userid, ts_jabber:choose_or_cache_user_id(0,"user","pwd")).



myset_env()->
    myset_env(0).
myset_env(Val)->
    application:set_env(stdlib,debug_level,Val).
