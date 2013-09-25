============
Introduction
============


What is Tsung?
===============

Tsung (formerly IDX-Tsunami) is a distributed load testing
tool. It is protocol-independent and can currently be used to stress
HTTP, WebDAV, SOAP, PostgreSQL, MySQL, LDAP and Jabber/XMPP servers.

It is distributed under the GNU General Public License version 2.


What is Erlang and why is it important for Tsung?
==================================================

Tsung's main strength is its ability to simulate a huge number of
simultaneous user from a single machine. When used on cluster, you can
generate a really impressive load on a server with a modest cluster,
easy to set-up and to maintain. You can also use Tsung on a cloud like
EC2.

Tsung is developed in Erlang and this is where the power of
Tsung resides.


Erlang is a *concurrency-oriented* programming language.
Tsung is based on the Erlang OTP (Open Transaction Platform) and
inherits several characteristics from Erlang:


Performance
  Erlang has been made to support hundred thousands of
  lightweight processes in a single virtual machine.

Scalability
  Erlang runtime environment is naturally distributed,
  promoting the idea of process's location transparency.

Fault-tolerance
  Erlang has been built to develop robust,
  fault-tolerant systems. As such, wrong answer sent from the server
  to Tsung does not make the whole running benchmark crash.


More information on Erlang on http://www.erlang.org.


Tsung background
================

History:

* Tsung development was started by Nicolas Niclausse in
  2001 as a distributed jabber load stress tool for internal use at
  http://IDEALX.com/ (now OpenTrust).  It has evolved as an open-source
  multi-protocol load testing tool several months later. The HTTP
  support was added in 2003, and this tool has been used for several
  industrial projects.  It is now hosted by Erlang-projects, and
  supported by http://process-one.net/. The list of contributors
  is available in the source archive at https://github.com/processone/tsung/blob/master/CONTRIBUTORS and at https://github.com/processone/tsung/graphs/contributors.

* It is an industrial strength implementation of a *stochastic model*
  for real users simulation. User events distribution is based on a Poisson Process. More information on this topic in:

  Z. Liu, N. Niclausse, and C. Jalpa-Villanueva.  **Traffic Model
  and Performance Evaluation of Web Servers**. *Performance Evaluation, Volume 46, Issue 2-3, October 2001*.

* This model has already been tested in the INRIA *WAGON*
  research prototype (Web trAffic GeneratOr and beNchmark). WAGON was
  used in the http://www.vthd.org/ project (Very High Broadband
  IP/WDM test platform for new generation Internet applications, 2000-2004).


Tsung has been used for very high load tests:

* *Jabber/XMPP* protocol:

  * 90,000 simultaneous Jabber users on a 4-node Tsung cluster (3xSun V240 + 1 Sun V440).
  * 10,000 simultaneous users. Tsung was running on a 3-computers cluster (CPU 800MHz).

* *HTTP and HTTPS* protocol:

  * 12,000 simultaneous users. Tsung were running on a 4-computers cluster (in 2003).
    The tested platform reached 3,000 requests per second.
  * 10 million simultaneous users running on a 75-computers cluster, generating more
    than one million requests per second.


Tsung has been used at:


* *DGI* (Direction Générale des impôts): French finance ministry

* *Cap Gemini Ernst & Young*

* *IFP* (Institut Français du Pétrole): French Research Organization
  for Petroleum

* *LibertySurf*

* *Sun* (TM) for their Mooddlerooms platform on Niagara processors: https://blogs.oracle.com/kevinr/resource/Moodle-Sun-RA.pdf

