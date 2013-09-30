.. index:: options
.. _sec-options-label:

Setting options
===============

.. index:: override
.. index:: thinktime
.. index:: ssl_ciphers
.. index:: tcp_snd_buffer
.. index:: tcp_rcv_buffer
.. index:: udp_snd_buffer
.. index:: udp_rcv_buffer

Thinktimes, SSL, Buffers
------------------------

Default values can be set-up globally: ``thinktime`` between requests
in the scenario, SSL cipher algorithms, TCP/UDP buffer sizes (the
default value is 32KB). These values overrides those set in session
configuration tags if override is true.

.. code-block:: xml

 <option name="thinktime" value="3" random="false" override="true"/>
 <option name="ssl_ciphers"
         value="EXP1024-RC4-SHA,EDH-RSA-DES-CBC3-SHA"/>
 <option name="tcp_snd_buffer" value="16384"></option>
 <option name="tcp_rcv_buffer" value="16384"></option>
 <option name="udp_snd_buffer" value="16384"></option>
 <option name="udp_rcv_buffer" value="16384"></option>


.. index:: idle_timeout
.. index:: global_ack_timeout

Timeout for acknowledgments of messages
---------------------------------------

This is used to set the idle timeout(used for 'parse' and 'local' ack) and
global ack timeout(used for 'global' ack). By default, idle timeout will be
10min(600000) and global ack timeout will be ``infinity``. This value
can be changed like this:

.. code-block:: xml

 <option name="idle_timeout" value="300000"></option>
 <option name="glocal_ack_timeout" value="6000000"></option>


.. index:: hibernate

Hibernate
---------

.. versionadded:: 1.3.1

The option ``hibernate`` is used to reduced memory consumption of
simulated users during thinktimes. By default, hibernation will be
activated for thinktimes higher than 10sec. This value can be changed
like this:

.. code-block:: xml

  <option name="hibernate" value="5"></option>


To disable hibernation, you must set the value to ``infinity``.

.. index:: rate_limit

Rate_limit
----------

.. versionadded:: 1.4.0

``rate_limit``. This will limit the bandwidth of each client
(using a token bucket algorithm). The value is in KBytes per
second. You can also specify a maximum burst value
(eg. ``max='2048'``). By default the burst size is the same as
the rate (1024KB in the following example). Currently, only incoming
traffic is rate limited.

.. code-block:: xml

  <option name="rate_limit" value="1024"></option>


Ports_range
-----------

If you need to open more than 30000 simultaneous connections on a
client machine, you will be limited by the number of TCP client ports,
even if you use several IPs (this is true at least on Linux). To
bypass this limit, Tsung must not delegate the selection of client
ports and together with using several IP for each client,
you have to defined a range for available clients ports, for ex:

.. code-block:: xml

  <option name="ports_range" min="1025" max="65535"/>


.. index:: seed

Setting the  seed for random numbers
------------------------------------

If you want to use a fixed seed for the random generator, you can use
the ``seed`` option, like this (by default, tsung will use the
current time to set the seed, therefore random numbers should be
different for every test).

.. code-block:: xml

  <option name="seed" value="42"/>


Path for Websocket
------------------

When you use Websocket as a server type, you can set the connect path
for Websocket as following:

.. code-block:: xml

  <option name="websocket_path" value="/chat"/>

.. _jabber-options-label:

XMPP/Jabber options
-------------------


Default values for specific protocols can be defined. Here is an
example of option values for Jabber/XMPP:

.. code-block:: xml

  <option type="ts_jabber" name="global_number" value="5" />
  <option type="ts_jabber" name="userid_max" value="100" />
  <option type="ts_jabber" name="domain" value="jabber.org" />
  <option type="ts_jabber" name="username" value="myuser" />
  <option type="ts_jabber" name="passwd" value="mypasswd" />
  <option type="ts_jabber" name="muc_service" value="conference.localhost"/>


Using these values, users will be ``myuserXXX`` where *XXX* is an integer in
the interval *[1:userid_max]* and passwd ``mypasswdXXX``

If not set in the configuration file, the values will be set to:

* global_number = 100
* userid_max    = 10000
* domain   = erlang-projects.org
* username = tsunguser
* passwd   = sesame


Other options are available if you prefer to use a CSV file for
username/password, see :ref:`sec-read-user-jabber-csv-label`.


You can also set the ``muc_service`` here (see previous example).


HTTP options
------------

For HTTP, you can set the ``UserAgent`` values
(**available since Tsung 1.1.0**), using a probability for each
value (the sum of all probabilities must be equal to 100)

.. code-block:: xml

  <option type="ts_http" name="user_agent">
    <user_agent probability="80">
       Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.7.8) Gecko/20050513 Galeon/1.3.21
    </user_agent>
    <user_agent probability="20">
      Mozilla/5.0 (Windows; U; Windows NT 5.2; fr-FR; rv:1.7.8) Gecko/20050511 Firefox/1.0.4
    </user_agent>
  </option>

