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


-vc('$Id$ ').
-author('jzhihui521@gmail.com').

%% use by the client to create the request
-record(websocket_request, {
          version = "13", % default is 13(rfc6455)
          type, % connect or message
          path, % connection path
          subprotos = [], % subprotocols
          data % websocket data
         }).

-record(websocket_dyndata, { 
          none
         }
       ).

-record(websocket_session, {
          status, % status of handshake
          accept  % Sec-Websocket-Accept header value
         }).

-record(http, {
          status,
          accept,
          headers=[],
          close
         }).

%% opcode of websocket
-define(OP_CONT, 0).
-define(OP_TEXT, 1).
-define(OP_BIN, 2).
-define(OP_CLOSE, 8).
-define(OP_PING, 9).
-define(OP_PONG, 10).
