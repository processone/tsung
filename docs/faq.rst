.. index:: faq
.. _faq:

==========================
Frequently Asked Questions
==========================


Can't start distributed clients: timeout error
==============================================

Most of the time, when a crash happened at startup without any traffic
generated, the problem arise because the main Erlang controller node cannot
create a "slave" Erlang virtual machine. The message looks like::

   Can't start newbeam on host 'XXXXX (reason: timeout) ! Aborting!


The problem is that the Erlang slave module cannot start a remote slave
node.

You can test this using this simple command on the controller node
(remotehost is the name of the client node)::

  >erl -rsh ssh -sname foo -setcookie mycookie

  Eshell V5.4.3 (abort with ^G)
  (foo@myhostname)1>slave:start(remotehost,bar,"-setcookie mycookie").


You should see this::

  {ok,bar@remotehost}

If you got ``{error,timeout}``, it can be caused by several problems:

* ssh in not working (you must have a key without passphrase, or
  use an agent)
* Tsung and Erlang are not installed on all clients nodes
* Erlang version or location (install path) is not the same on all clients nodes
* A firewall is dropping Erlang packets: Erlang virtual machines use
  several TCP ports (dynamically generated) to communicate (if you are
  using EC2, you may have to change the Security Group that is applied on the VMs used
  for Tsung: open port range 0 - 65535)
* SELinux: You should disable SELinux on all clients.
* Bad :file:`/etc/hosts`:
  This one is wrong (real hostname should not refer to localhost/loopback)::

    127.0.0.1 localhost myhostname

  This one is good::

    127.0.0.1 localhost
    192.168.3.2 myhostname

* sshd configuration:
  For example, for SuSE 9.2 sshd is compiled with restricted set of
  paths (ie. when you shell into the account you get the users shell,
  when you execute a command via ssh you don't) and this makes it
  impossible to start an Erlang node (if Erlang is installed in
  :file:`/usr/local` for example).

  Run::

    ssh myhostname erl

  If the Erlang shell doesn't start then check what paths sshd was compiled with
  (in SuSE see :file:`/etc/ssh/sshd_config`) and symlink from one of the approved paths
  to the Erlang executable (thanks to Gordon Guthrie for reporting this).
* old beam processes (Erlang virtual machines) running on client nodes: kill all
  beam processes before starting Tsung.



Note that you do not need to use the ``127.0.0.1`` address in the configuration file.
It will not work if you use it as the injection interface. The shortname
of your client machine should not refer to this address.

**Warning** Tsung launches a new Erlang virtual machine to do the actual injection
even when you have only one machine in the injection cluster (unless
``use_controller_vm`` is set to true). This is because it
needs to by-pass some limit with the number of open socket from a
single process (1024 most of the time). The idea is to have several
system processes (Erl beam) that can handle only a small part of the
network connection from the given computer. When the
``maxusers`` limit (simultaneous) is reach, a new Erlang beam
is launched and the newest connection can be handled by the new beam).

**New in 1.1.0**: If you don't use the distributed feature of
Tsung and have trouble to start a remote beam on a local machine,
you can set the ``use_controller_vm`` attribute to true::

  <client host="mymachine" use_controller_vm="true">


Tsung crashes when I start it
=============================

Does your Erlang system has SSL support enabled ?

to test it::

  > erl
  Eshell V5.2  (abort with ^G)
  1> ssl:start().
  you should see 'ok'


.. _faq-emfile-label:

Why do i have error_connect_emfile errors?
==========================================

:index:`emfile` error means : **too many open files**

This happens usually when you set a high value for :ref:`maxusers-label`
(in the ``<client>`` section) (the default value is 800).

The errors means that you are running out of file descriptors; you
must check that :ref:`maxusers-label` is less than the maximum number of
file descriptors per process in your system (see :command:`ulimit -n`).

You can either raise the limit of your operating system (see
:file:`/etc/security/limits.conf` for Linux) or decrease :ref:`maxusers-label`
Tsung will have to start several virtual machine on the same host to
bypass the maxusers limit.

It could be good if you want to test a large number of users to make some
modifications to your system before launching Tsung:

* Put the domain name into :file:`/etc/hosts` if you don't want the DNS
  overhead and you only want to test the target server
* Increase the maximum number of open files and customize TCP settings in
  :file:`/etc/sysctl.conf`. For example::

    net.ipv4.tcp_tw_reuse = 1
    net.ipv4.tcp_tw_recycle = 1
    net.ipv4.ip_local_port_range = 1024 65000
    fs.file-max = 65000


Tsung still crashes/fails when I start it!
==========================================

First look at the log file
:file:`~/.tsung/log/XXX/tsung_controller@yourhostname` to see if there
is a problem.

If the file is not created and a crashed dump file is present, maybe
you are using a binary installation of Tsung not compatible with the
version of Erlang you used.

If you see nothing wrong, you can compile Tsung with full
debugging: recompile with :command:`make debug`, and
don't forget to set the loglevel to ``debug`` in the XML file
(see :ref:`tsung.xml log levels <sec-file-structure-label>`).

To start the debugger or see what happen, start Tsung with the
``debug`` argument instead of ``start``. You will have
an Erlang shell on the ``tsung_controller`` node. Use
:command:`toolbar:start().` to launch the graphical tools provided by
Erlang.

Can I dynamically follow redirect with HTTP?
============================================

If your HTTP server sends 30X responses (:index:`redirect`) with dynamic URLs,
you can handle this situation using a dynamic variable:

.. code-block:: xml

   <request>
     <dyn_variable name="redirect" re="Location: (http://.*)\r"/>
     <http url="index.html" method="GET" ></http>
   </request>

   <request subst="true">
     <http url="%%_redirect%%" method="GET"></http>
   </request>

You can even handle the case where the server use several redirections
successively using a repeat loop (this works only with version 1.3.0 and up):

.. code-block:: xml

  <request>
    <dyn_variable name="redirect" re="Location: (http://.*)\r"/>
    <http url='/test/redirect.html' method='GET'></http>
  </request>

  <repeat name="redirect_loop" max_repeat="5">
    <request subst="true">
      <dyn_variable name="redirect" re="Location: (http://.*)\r"/>
      <http url="%%_redirect%%" method="GET"></http>
    </request>
    <until var="redirect" eq=""/>
  </repeat>



.. _what-format-stats:

What is the format of the stats file tsung.log?
===============================================


Sample tsung.log::

  # stats: dump at 1218093520
  stats: users 247 247
  stats: connected 184 247
  stats: users_count 184 247
  stats: page 187 98.324 579.441 5465.940 2.177 9.237 595 58
  stats: request 1869 0.371 0.422 5.20703125 0.115 0.431 7444062 581
  stats: connect 186 0.427 0.184 4.47216796875 0.174 0.894 88665254 59
  stats: tr_login 187 100.848 579.742 5470.223 2.231 56.970 91567888 58
  stats: size_rcv 2715777 3568647
  stats: 200 1869 2450
  stats: size_sent 264167 347870
  # stats: dump at 1218093530
  stats: users 356 356
  stats: users_count 109 356
  stats: connected -32 215
  stats: page 110 3.346 0.408 5465.940 2.177 77.234 724492 245
  stats: request 1100 0.305 0.284 5.207 0.115 0.385 26785716 2450
  stats: connect 110 0.320 0.065 4.472 0.174 0.540 39158164 245
  stats: tr_login 110 3.419 0.414 5470.223 2.231 90.461 548628831 245
  stats: size_rcv 1602039 5170686
  stats: 200 1100 3550
  stats: size_sent 150660 498530
  ...


the format is, for ``request``, ``page``, ``session`` and transactions ``tr_XXX``::

  stats: name, 10sec_count, 10sec_mean, 10sec_stddev, max, min, mean, count

or for HTTP returns codes, ``size_sent`` and ``size_rcv``::

  stats: name, count(during the last 10sec), totalcount(since the beginning)

How can I compute percentile/quartiles/median for transactions or requests response time?
=========================================================================================

It's not directly possible. But since **version 1.3.0**, you can
use a new experimental statistic backend: set ``backend="fullstats"`` in the
``<tsung>`` section of your configuration file (also see :ref:`sec-file-structure-label`).

This will print every statistics data in a raw format in a file named
:file:`tsung-fullstats.log`. **Warning**: this may impact the performance of
the controller node (a lot of data has to be written to disk).

The data looks like::

 {sum,connected,1}
 {sum,connected,-1}
 [{sample,request,214.635},
  {sum,size_rcv,268},
  {sample,page,831.189},
  {count,200},
  {sum,size_sent,182},
  {sample,connect,184.787},
  {sample,request,220.974},
  {sum,size_rcv,785},
  {count,200},
  {sum,size_sent,164},
  {sample,connect,185.482}]
 {sum,connected,1}
 [{count,200},{sum,size_sent,161},{sample,connect,180.812}]
 [{sum,size_rcv,524288},{sum,size_rcv,524288}]


Since version **1.5.0**, a script :command:`tsung_percentile.pl` is
provided to compute the percentiles from this file.

How can I specify the number of concurrent users?
=================================================

You can't. But it's on purpose: the load generated by
Tsung is dependent on the arrival time between new
clients. Indeed, once a client has finished his session in
Tsung, it stops. So the number of concurrent users is
a function of the arrival rate and the mean session duration.

For example, if your web site has 1,000 visits/hour, the arrival rate
is ``1000/3600 = 0.2778`` visits/second. If you want to simulate the same
load, set the inter-arrival time is to ``1/0.27778 = 3.6 sec`` (e.g. ``<users
interarrival="3.6" unit="second">`` in the ``arrivalphase`` node in the
XML config file).

.. _sec-faq-snmp-label:

SNMP monitoring doesn't work?!
==============================


It use SNMP v1 and the "public" community. It has been tested with
http://net-snmp.sourceforge.net/.

You can try with :command:`snmpwalk` to see if your snmpd config is ok::

 >snmpwalk -v 1 -c public IP-OF-YOUR-SERVER .1.3.6.1.4.1.2021.4.5.0
 UCD-SNMP-MIB::memTotalReal.0 = INTEGER: 1033436

SNMP doesn't work with Erlang R10B and Tsung older than 1.2.0.

There is a small bug in the ``snmp_mgr`` module in old Erlang
release (R9C-0). This is fixed in Erlang R9C-1 and up, but you can apply this patch to make it
work on earlier version::

  --- lib/snmp-3.4/src/snmp_mgr.erl.orig  2004-03-22 15:21:59.000000000 +0100
  +++ lib/snmp-3.4/src/snmp_mgr.erl       2004-03-22 15:23:46.000000000 +0100
  @@ -296,6 +296,10 @@
       end;
   is_options_ok([{recbuf,Sz}|Opts]) when 0 < Sz, Sz =< 65535 ->
       is_options_ok(Opts);
  +is_options_ok([{receive_type, msg}|Opts]) ->
  +    is_options_ok(Opts);
  +is_options_ok([{receive_type, pdu}|Opts]) ->
  +    is_options_ok(Opts);
   is_options_ok([InvOpt|_]) ->
       {error,{invalid_option,InvOpt}};
   is_options_ok([]) -> true.


How can i simulate a fix number of users?
=========================================

Use ``maxnumber`` to set the max number of concurrent users in a
phase, and if you want Tsung to behave like ab, you can use a loop
in a session (to send requests as fast as possible); you can also define a
max ``duration`` in ``<load>``.


.. code-block:: xml

 <load duration="5" unit="minute">
    <arrivalphase phase="1" duration="10" unit="minute">
    <users maxnumber="10" arrivalrate="100" unit="second"></users>
 </arrivalphase>
 </load>
 <sessions>
   <session probability="100" name="ab">
     <for from="1" to="1000" var="i">
       <request>
         <http url="http://myserver/index.html" method="GET"></http>
       </request>
     </for>
   </session>
 </sessions>

