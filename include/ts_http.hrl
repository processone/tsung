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

-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

%% use by the client to create the request
-record(http_request, {url, cookie=none, method=get, body=[], id = 0 }).

%% use by the client process to store information about the current request during 
%% the parsing of the response
-record(http, {content_length= 0, % HTTP header: content length
			   body_size     = 0, % current size of body,
			   chunk_toread  = 0, % chunk data to be read
			   status        = none  % HTTP resp. status :200, etc. 'none' if no current cnx.
			  }).

%% HTTP Protocol
-define(GET, "GET").
-define(POST, "POST").

-define(USER_AGENT, "IDX-Tsunami").

