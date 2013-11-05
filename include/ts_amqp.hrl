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

-define(AMQP_USER, "guest").
-define(AMQP_PASSWORD, "guest").

-define(PROTOCOL_VERSION_MAJOR, 0).
-define(PROTOCOL_VERSION_MINOR, 9).
-define(PROTOCOL_HEADER, <<"AMQP", 0, 0, 9, 1>>).
-define(PROTOCOL, rabbit_framing_amqp_0_9_1).
-define(MAX_CHANNEL_NUMBER, 65535).

-define(CLIENT_CAPABILITIES, [{<<"publisher_confirms">>,         bool, true},
                              {<<"exchange_exchange_bindings">>, bool, true},
                              {<<"basic.nack">>,                 bool, true},
                              {<<"consumer_cancel_notify">>,     bool, true}]).

%% use by the client to create the request
-record(amqp_request, {
          version = "0_9_1", % default is 0.9.1
          type,
          username,
          password,
          heartbeat,
          vhost,
          channel = "1",
          exchange,
          routing_key,
          confirm,
          prefetch_size,
          prefetch_count,
          persistent,
          payload,
          payload_size,
          queue,
          ack
         }).

-record(amqp_dyndata, { 
          none
         }
       ).

-record(ch, {
          ack,
          next_pub_seqno,
          unconfirmed_set
         }
       ).

-record(amqp_session, {
          vhost,
          protocol, 
          channel_max = 65535,
          map_num_pa,
          ack_buf,
          status, % status of handshake
          waiting = none % waiting state: {Channel, Expecting}
         }).

