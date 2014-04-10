%%%  This code was developped by Zhihui Jiao(jzhihui521@gmail.com).
%%%
%%%  Copyright (C) 2013 Zhihui Jiao
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
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.


-vc('$Id$ ').
-author('jzhihui521@gmail.com').

%% use by the client to create the request
-record(mqtt_request, {
          type,
          clean_start = true,
          keepalive = 10, % 10s
          will_topic,
          will_qos,
          will_msg,
          will_retain,
          topic,
          qos = 0,
          retained = false,
          payload,
          username,
          password
         }).

-record(mqtt_dyndata, {
          none
         }
       ).

-record(mqtt_session, {
          ack_buf = <<>>,
          ping_pid,
          keepalive,
          curr_id = 0,
          wait, % wait code
          status % connection status
         }).
