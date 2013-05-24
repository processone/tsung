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
          vhost,
          exchange,
          routing_key,
          confirm,
          prefetch_size,
          prefetch_count,
          persistent,
          size,
          queue,
          ack
         }).

-record(amqp_dyndata, { 
          none
         }
       ).

-record(amqp_session, {
          vhost,
          protocol, 
          channel,
          next_pub_seqno,
          unconfirmed_set,
          ack,
          ack_buf,
          status % status of handshake
         }).

