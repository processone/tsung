==================
Benchmark Approach
==================

HTTP/WebDAV
===========

Benchmarking a Web server
-------------------------

#. Record one or more sessions: start the recorder with:
   :command:`tsung-recorder start`, and then configure your browser to use Tsung
   proxy recorder (the listen port is 8090). A session file will be
   created. For HTTPS recording, use ``http://-`` instead of
   ``https://`` in your browser.

#. Edit / organize scenario, by adding recorded sessions in the
   configuration file.

#. Write small code for dynamic parts if needed and place dynamic mark-up
   in the scenario.

#. Test and adjust scenario to have a nice progression of the load. This
   is highly dependent of the application and of the size of the target
   server(s). Calculate the normal duration of the scenario and use the
   interarrival time between users and the duration of the phase to estimate
   the number of simultaneous users for each given phase.

#. Launch benchmark with your first application parameters setup:
   :command:`tsung start` (run :command:`man tsung` for more options).

#. Wait for the end of the test or stop by hand with
   :command:`tsung stop` (reports can also be generated during the
   test (see :ref:`statistics-reports`): the statistics are
   updated every 10 seconds). For a brief summary of the current
   activity, use :command:`tsung status`.

#. Analyze results, change parameters and relaunch another benchmark.


WebDAV
------

It's the same approach as HTTP: first you start to record one or more
sessions with the :ref:`recorder <tsung-recorder>`:
:command:`tsung-recorder -p webdav start`.

Benchmarking a proxy server
---------------------------

By default, the HTTP plugin is used to benchmark HTTP servers. But you
can also benchmark HTTP Proxy servers. To do that, you must add in the
``options`` section:

.. index:: ts_http, http_use_server_as_proxy
.. code-block:: xml

  <option type="ts_http" name="http_use_server_as_proxy" value="true"></option>


LDAP
====

An LDAP plugin for the recorder is not yet implemented, so you have to
write the session by yourself; see section :ref:`sec-session-ldap-label` for
more information.

PostgreSQL
==========

It's the same approach as HTTP: first you start to record one or more
sessions with the recorder: :command:`tsung-recorder -p pgsql start`.

This will start a proxy listening to port 8090 and will proxy requests
to ``127.0.0.0:5432``.

To choose another port and/or address:
:command:`tsung-recorder -L 5432 -I 10.6.1.1 -P 5433 -p pgsql start`.

This will start a proxy listening to port 5432 and will proxy requests
to ``10.6.1.1:5433``.

MySQL
=====

A MySQL plugin for the recorder is not yet implemented, so you have to
write the session by yourself; see section :ref:`session-mysql-label` for
more information.

Jabber/XMPP
===========

Overview
--------

This paragraph explains how to write a session for Jabber/XMPP.

There are two differences between HTTP and Jabber testing:

* There is no recorder for Jabber, so you have to write your
  sessions by hand. An example is provided in
  :ref:`sec-session-jabber-label`.

* The Jabber plugin does not parse XML; instead it uses packet
  acknowledgments.


Acknowledgments of messages
---------------------------

Since the Jabber plugin does not parse XML (historically, it was for
performance reasons), you must have a way to tell when a request is
finished. There are 3 possibilities using the ``ack`` attribute:

* ``ack="local"`` as soon as a packet is received from the server, the
  request is considered as completed. Hence if you use a local ack with a request
  that do not require a response from the server (presence for ex.), it
  will wait forever (or until a timeout is reached).

* ``ack="no_ack"`` as soon as the request is send, it is considered as completed (do
  not wait for incoming data).

* ``ack="global"`` synchronized users. its main use is for waiting for all
  users to connect before sending messages. To do that, set a request
  with global ack (it can be the first presence msg:

  .. index:: presence
  .. code-block:: xml

    <request> <jabber type="presence" ack="global"/> </request>

  You also have to specify the number of users to be connected:

  .. index:: ts_jabber, global_number
  .. code-block:: xml

    <option type="ts_jabber" name="global_number" value="100"></option>

  To be sure that exactly ``global_number`` users are started, add the
  ``maxnumber`` attribute to ``users``:

  .. index:: maxnumber, interarrival
  .. code-block:: xml

    <users maxnumber="100" interarrival="1.0" unit="second"></users>

  If you do not specify ``maxnumber``, the global ack will be reset every
  ``global_number`` users.


.. _bidi-presence-label:

Bidirectional Presence
^^^^^^^^^^^^^^^^^^^^^^

**New in 1.2.2**: This version adds an new option for a
session. if you set the attribute ``bidi`` (for bidirectional)
in the ``<session>`` tag: ``<session ... bidi="true">``,
then incoming messages from the server will be analyzed. Currently,
only roster subscription requests are handled: if a user received a
subscription request (``<presence ... type="subscribe">``), it
will respond with a ``<presence ... type="subscribed">``
message.

Status: Offline, Connected and Online
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can send messages to offline or online users. A user is considered
online when he has send a ``presence:initial`` message (before
this message , the state of the user is ``connected``).

If you want to switch back to **connected** before going
**offline**, you can use a **presence:final** message:

**presence:final** does two things:

* It removes the client from the list of Online users, and moves
  them into the list of Connected users.
* It sends a broadcast presence update of ``type="unavailable"``.


**presence:final** is optional.

**Warning: this is new in 1.2.0**, in earlier version, only 2
status were available: online and offline; a user was considered
online as soon as it was connected.

Authentication
--------------

Below are configuration examples for the possible authentication
methods.  Note: the regular expressions used here are only examples -
they may need to be altered depending on how a particular server
implementation composes messages (see also :ref:`jabber-options-label`
for password settings).

* **plain authentication** - sends clear-text passwords:

  .. code-block:: xml

     <session probability="100" name="jabber-plain" type="ts_jabber">

       <request> <jabber type="connect" ack="local"></jabber> </request>

       <thinktime value="2"></thinktime>

       <transaction name="auth_plain">
         <request> <jabber type="auth_get" ack="local"></jabber> </request>
         <request> <jabber type="auth_set_plain" ack="local"></jabber> </request>
       </transaction>
       ...
     </session>

* **digest authentication** as described in XMPP JEP-0078: Non-SASL Authentication
  http://www.jabber.org/jeps/jep-0078.html

  .. code-block:: xml

     <session probability="100" name="jabber-digest" type="ts_jabber">

       <!-- regexp captures stream ID returned by server -->
       <request>
         <dyn_variable name="sid" re="&lt;stream:stream id=&quot;(.*)&quot; xmlns:stream"/>
         <jabber type="connect" ack="local"></jabber>
       </request>

       <thinktime value="2"></thinktime>

       <transaction name="auth_digest">
         <request> <jabber type="auth_get" ack="local"></jabber> </request>
         <request subst="true"> <jabber type="auth_set_digest" ack="local"></jabber> </request>
       </transaction>
       ...
     </session>

* **sip-digest authentication**

  .. code-block:: xml

     <session probability="100" name="jabber-sipdigest" type="ts_jabber">

     <request> <jabber type="connect" ack="local"></jabber> </request>

     <thinktime value="2"></thinktime>

    <transaction name="auth_sipdigest">
      <!-- regexp captures nonce value returned by server -->
      <request>
        <dyn_variable name="nonce"
          re="&lt;Nonce encoding=&quot;hex&quot;&gt;(.*)&lt;\/Nonce&gt;"/>
        <jabber type="auth_get" ack="local"></jabber>
      </request>
      <request subst="true"> <jabber type="auth_set_sip" ack="local"></jabber> </request>
    </transaction>
    ...
    </session>


Privacy list testing
--------------------

There are two actions available to allow for rudimentary privacy lists
load testing:

* **privacy:get_names** gets the list of all names
  .. of privacy lists stored by the server for a given user

* **privacy:set_active** sets a list with a predefined
  name as active. The list name is determined from the JID,
  e.g. if the user's JID is "john@average.com" then the list name
  is "john@average.com_list". One should take care of properly seeding
  the server database in order to ensure that such a list exists.

