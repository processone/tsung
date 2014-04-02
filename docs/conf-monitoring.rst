Monitoring
==========

Tsung is able to monitor remote servers using several backends that
communicates with remote agent. This is configured in the ``<monitoring>`` section. Available
statistics are: CPU activity, load average and memory usage.

Note that you can get the nodes to monitor from a job scheduler, like:

.. code-block:: xml

  <monitor batch="true" host="torque" type="erlang"></monitor>



Several types of remote agents are supported (``erlang`` is the default):


Erlang
------

The remote agent is started by Tsung. It use erlang communications to
retrieve statistics of activity on the server. For example, here is a
cluster monitoring definition based on Erlang agents, for a cluster of
6 computers:

.. code-block:: xml

  <monitoring>
    <monitor host="geronimo" type="erlang"></monitor>
    <monitor host="bigfoot-1" type="erlang"></monitor>
    <monitor host="bigfoot-2" type="erlang"></monitor>
    <monitor host="f14-1" type="erlang"></monitor>
    <monitor host="f14-2" type="erlang"></monitor>
    <monitor host="db" type="erlang"></monitor>
  </monitoring>



.. note::

 monitored computers needs to be accessible through the network, and
 erlang communications must be allowed (no firewall is better). SSH
 (or rsh) needs to be configured to allow connection without password
 on. **You must use the same version of Erlang/OTP on all nodes
 otherwise it may not work properly!**

If you can't have erlang installed on remote servers, you can use one
of the other available agents.

.. versionadded:: 1.5.1

erlang monitoring includes now an option to monitor a mysql db with
mysqladmin. Use it like this:

.. code-block:: xml

    <monitor host="db" type="erlang"></monitor>
     <mysqladmin port="3306" username="root" password="sesame" />
    </monitor>

Availabe stats: number of mysql threads and Questions (queries)

.. index:: snmp

SNMP
----

The ``type`` keyword ``snmp`` can replace the erlang keyword, if SNMP monitoring
is preferred. They can be mixed. **Since version 1.2.2**, you can customize the SNMP version,
community and port number. It uses the Management Information Base (MIB) provided in
``net-snmp`` (see also :ref:`sec-faq-snmp-label`).

.. code-block:: xml

  <monitoring>
    <monitor host="geronimo" type="snmp"/>
    <monitor host="f14-2" type="erlang"></monitor>
    <monitor host="db" type="snmp">
      <snmp version="v2" community="mycommunity" port="11161"/>
    </monitor>
  </monitoring>


The default ``version`` is ``v1``, default community
``public`` and default port ``161``.

Since version **1.4.2**, you can also customize the object identifiers (OID)
retrieved from the SNMP server, using one or several ``oid``
element:

.. code-block:: xml

 <monitor host="127.0.0.1" type="snmp">
   <snmp version="v2">
     <oid value="1.3.6.1.4.1.42.2.145.3.163.1.1.2.11.0"
          name="heapused" type="sample" eval="fun(X)-> X/100 end."/>
   </snmp>
 </monitor>


``type`` can be ``sample``, ``counter`` or
``sum``, and optionally you can define a function (with erlang
syntax) to be applied to the value (``eval`` attribute).

.. index:: munin

Munin
-----

.. versionadded:: 1.3.1

Tsung is able to retrieve data from a munin-node agent
(see http://munin-monitoring.org/wiki/munin-node). The ``type``
keyword must be set to ``munin``, for example:

.. code-block:: xml

  <monitoring>
    <monitor host="geronimo" type="munin"/>
    <monitor host="f14-2" type="erlang"></monitor>
  </monitoring>
