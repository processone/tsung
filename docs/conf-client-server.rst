Clients and server
==================

Scenarios start with clients (Tsung cluster) and server definitions:

Basic setup
-----------

.. index:: client
.. index:: host
.. index:: use_controller_vm
.. index:: server


For non distributed load, you can use a basic setup like:

.. code-block:: xml

 <clients>
   <client host="localhost" use_controller_vm="true"/>
 </clients>

 <servers>
   <server host="192.168.1.1" port="80" type="tcp"></server>
 </servers>


This will start the load on the same host and on the same Erlang
virtual machine as the controller.

The server is the entry point into the cluster. You can add several
servers, by default each server will have a :index:`weight` of 1, and each
session will choose a server randomly according to the weight. You can
set a weight for each server like this (weight can be an integer or
a float):

.. code-block:: xml

 <servers>
   <server host="server1" port="80" type="tcp" weight="4"></server>
   <server host="server2" port="80" type="tcp" weight="1"></server>
 </servers>


(in version older than **1.5.0**, the ``weight`` option was
not implemented and a round robin algorithm was used to choose the
server).

*Type* can be ``tcp``, ``ssl``,
``udp`` (for IPv6, use ``tcp6``, ``ssl6`` or
``udp6`` ; only available in  version **1.4.2** and newer)
or ``websocket`` (only available in version **1.5.0** and newer))

There's also a specific type fo BOSH: ``bosh`` for unencrypted BOSH, and ``bosh_ssl`` for encrypted connection

.. index:: cpu

Advanced setup
--------------

The next example is more complex, and use several features for
advanced distributed testing:

.. code-block:: xml

 <clients>
   <client host="louxor" weight="1" maxusers="800">
     <ip value="10.9.195.12"></ip>
     <ip value="10.9.195.13"></ip>
   </client>
   <client host="memphis" weight="3" maxusers="600" cpu="2"/>
 </clients>

 <servers>
   <server host="10.9.195.1" port="8080" type="tcp"></server>
 </servers>

.. index:: ip

Several virtual IP can be used to simulate more machines. This is
very useful when a load-balancer use the client's IP to
distribute the traffic among a cluster of servers. **New in 1.1.1**:
IP is no longer mandatory. If not specified, the default IP will be
used.


**New in 1.4.0:** You can use ``<ip scan="true" value="eth0"/>`` to scan for all the IP aliases on a given interface
(``eth0`` in this example).

In this example, a second machine is used in the Tsung cluster,
with a higher weight, and 2 cpus. Two Erlang virtual machines will be
used to take advantage of the number of CPU.

.. note::

   Even if an Erlang VM is now able to handle several CPUs
   (erlang SMP), benchmarks shows that it's more efficient to use one VM
   per CPU (with SMP disabled) for tsung clients. Only the controller node is using SMP
   erlang. Therefore, ``cpu`` should be equal to the number of cores of
   your nodes. If you prefer to use erlang SMP, add the ``-s``
   option when starting tsung (and don't set ``cpu`` in the config
   file).


By default, the load is distributed uniformly on all CPU (one CPU
per client by default). The weight parameter (integer) can be used to
take into account the speed of the client machine. For instance, if
one real client has a weight of 1 and the other client has a weight
of 2, the second one will start twice the number of users as the
first (the proportions will be 1/3 and 2/3). In the earlier example
where for the second client has 2 CPU and weight=3, the weight is
equal to 1.5 for each CPU.

.. index:: maxusers

.. _maxusers-label:

maxusers
^^^^^^^^

The ``maxusers`` parameter is used to bypass the limit of maximum
number of sockets opened by a single process (1024 by default on many
OS) and the lack of scalability of the ``select`` system call. When
the number of users is higher than the limit, a new erlang virtual
machine will be started to handle new users. The default value of
``maxusers`` is 800. Nowadays, with kernel polling enable, you can and
should use a very large value for ``maxusers`` (30000 for example)
without performance penalty (but don't forget to raise the limit of
the OS with :command:`ulimit -n`, see also :ref:`faq-emfile-label`).

.. note:: 

   If you are using a tsung master with slaves, the master 
   distributes sessions to slaves. If a session contains multiples requests, 
   a slave will execute each of these requests in order.


Running Tsung with a job scheduler
----------------------------------

.. index:: batch

Tsung is able to get its client node list from a batch/job
:index:`scheduler`. It currently handle PBS/torque, LSF and OAR. To do this,
set the ``type`` attribute to ``batch``, e.g.:

.. code-block:: xml

  <client type="batch" batch="torque" maxusers="30000">

.. index:: scan_intf

If you need to scan IP aliases on nodes given by the batch scheduler,
use *scan_intf* like this:

.. code-block:: xml

  <client type="batch" batch="torque" scan_intf='eth0' maxusers="30000">
