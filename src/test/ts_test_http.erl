%%%
%%%  Copyright © Nicolas Niclausse 2007
%%%
%%%	 Author : Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
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

-module(ts_test_http).
-vc('$Id: ts_test_jabber.erl 768 2007-11-15 11:01:01Z mremond $ ').
-author('Nicolas.Niclausse@niclux.org').

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_http.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->ok.

subst_redirect_test()->
    myset_env(),
    URL="%%_redirect%%",
    Cookie=#cookie{domain="erlang.org",path="/",key="toto",value="bar"},
    Proto=#http_dyndata{cookies=[Cookie],user_agent="Firefox"},
    DynVars=ts_dynvars:new(redirect,"http://erlang.org/bidule/truc"),
    {Req,_}=ts_http:add_dynparams(true,#dyndata{proto=Proto,dynvars=DynVars},
                                  #http_request{url=URL},
                                  {"erlang.org",80}),
    Str="GET /bidule/truc HTTP/1.1\r\nHost: erlang.org:80\r\nUser-Agent: Firefox\r\nCookie: toto=bar\r\n\r\n",
    ?assertMatch(Str, binary_to_list(ts_http:get_message(Req))).

 myset_env()->
    application:set_env(stdlib,debug_level,0).
