========
Features
========


Tsung main features
===================

* *High Performance*: ``Tsung`` can simulate a huge number of
  simultaneous users per physical computer: It can simulates thousands
  of users on a single CPU (Note: a simulated user is not always
  active: it can be idle during a ``thinktime``
  period). Traditional injection tools can hardly go further than a 
  few hundreds (Hint: if all you want to do is requesting a single URL
  in a loop, use :program:`ab`; but if you want to build complex
  scenarios with extended reports, ``Tsung`` is for you).

* *Distributed*: the load can be distributed on a cluster of client machines

* *Multi-Protocols* using a plug-in system: HTTP (both standard web
  traffic and SOAP), WebDAV, Jabber/XMPP and PostgreSQL are currently
  supported. LDAP and MySQL plugins were first included in the 1.3.0 release.

* *SSL* support

* *Several IP addresses* can be used on a single machine using the underlying OS IP Aliasing

* *OS monitoring* (CPU, memory and network traffic) using Erlang agents on remote servers or *SNMP*

* *XML configuration system*: complex user's scenarios are
  written in XML. Scenarios can be written with a simple browser using the
  Tsung recorder (HTTP and PostgreSQL only).

* *Dynamic scenarios*: You can get dynamic data from the
  server under load (without writing any code) and re-inject it in
  subsequent requests. You can also loop, restart or stop a
  session when a string (or regexp) matches the server response.

* *Mixed behaviours*: several :ref:`sessions <sessions-label>` can be used to simulate
  different type of users during the same benchmark. You can define
  the proportion of the various behaviours in the benchmark scenario.

* *Stochastic processes*: in order to generate a realistic
  traffic, user thinktimes and the arrival rate can be randomized
  using a probability distribution (currently exponential)


HTTP related features
=====================


* HTTP/1.0 and HTTP/1.1 support

* GET, POST, PUT, DELETE, HEAD, OPTIONS and PATCH requests

* Cookies: Automatic cookies management (but you can also manually add
  more cookies)

* 'GET If-modified since' type of request

* WWW-authentication Basic and Digest. OAuth 1.0

* User Agent support

* Any HTTP Headers can be added

* Proxy mode to record sessions using a Web browser

* SOAP support using the HTTP mode (the SOAPAction HTTP header is
  handled).

* HTTP server or proxy server load testing.


WEBDAV related features
=======================

The WebDAV (:RFC:`4918`) plugin is a superset of the HTTP plugin. It adds the
following features (some versionning extensions to WebDAV (:RFC:`3253`)
are also supported):


* Methods implemented: DELETE, CONNECT, PROPFIND, PROPPATCH, COPY,
  MOVE, LOCK, UNLOCK, MKCOL, REPORT, OPTIONS, MKACTIVITY, CHECKOUT, MERGE

* Recording of DEPTH, IF, TIMEOUT OVERWRITE, DESTINATION, URL and
  LOCK-TOKEN Headers.


Jabber/XMPP related features
============================

* Authentication (plain-text, digest and sip-digest). STARTTLS

* presence and register messages

* Chat messages to online or offline users

* MUC: join room, send message in room, change nickname

* Roster set and get requests

* Global users' synchronization can be set on specific actions

* BOSH & XMPP over Websocket

* raw XML messages

* PubSub

* Multiple vhost instances supported

* privacy lists: get all privacy list names, set list as active


PostgreSQL related features
===========================

* Basic and MD5 Authentication
* Simple Protocol
* Extended Protocol (new in version **1.4.0** )
* Proxy mode to record sessions


MySQL related features
======================
This plugin is experimental. It works only with MySQL version 4.1 and higher.

* Secured Authentication method only (MySQL >= 4.1)

* Basic Queries


Websocket related features
==========================

This plugin is experimental. It only supports :RFC:`6455` currently.
For used as a server type, it works like other transport protocol like
tcp and udp, any application specific protocol data can be send on it.

You can find examples used as session type in examples/websocket.xml.

* Both as a server type and session type


AMQP related features
=====================

This plugin is experimental. It only supports AMQP-0.9.1 currently.
You can find examples in examples/amqp.xml.

* Basic publish and consume

* Publisher confirm and consumer ack

* QoS

MQTT related features
=====================

This plugin is experimental. It supports MQTT V3.1.
You can find examples in examples/mqtt.xml.

* Connect to mqtt broker with options

* Publish mqtt messages to the broker

* Subscribe/unsubscribe topics

* Support QoS 0 and QoS 1

LDAP related features
=====================

* Bind

* Add, modify and search queries

* Starttls

Raw plugin related features
===========================

* TCP / UDP / SSL compatible

* raw messages

* no_ack, local or global ack for messages


Complete reports set
====================

Measures and statistics produced by Tsung are extremely feature-full.
They are all represented as a graphic. ``Tsung`` produces
statistics regarding:


* *Performance*: response time, connection time,  decomposition of the
  user scenario based on request grouping  instruction (called
  *transactions*), requests per second

* *Errors*: Statistics on page return code to trace errors

* *Target server behaviour*: An Erlang agent can gather information
  from the target server(s). Tsung produces graphs for CPU and memory
  consumption and network traffic. SNMP and munin is also supported to
  monitor remote servers.

\par Note that ``Tsung`` takes care of the synchronization process by itself. Gathered statistics are «synchronized».

It is possible to generate graphs during the benchmark as statistics are gathered in real-time.

Highlights
==========

``Tsung`` has several advantages over other injection tools:


* *High performance* and *distributed benchmark*: You can use Tsung to simulate tens of thousands of virtual users.

* *Ease of use*: The hard work is already done for all supported
  protocol. No need to write complex scripts. Dynamic scenarios only requires small trivial piece of code.

* *Multi-protocol support*: ``Tsung`` is for example one of the only tool to benchmark SOAP applications

* *Monitoring* of the target server(s) to analyze the behaviour and
  find bottlenecks. For example, it has been used to analyze cluster
  symmetry (is the load properly balanced ?) and to determine the best
  combination of machines on the three cluster tiers (Web engine, EJB
  engine and database)


