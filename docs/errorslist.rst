===========
Errors list
===========


error_closed
------------

Only for non persistent session (XMPP); the server unexpectedly closed
the connection; the session is aborted.

error_inet_<ERRORNAME>
----------------------

Network error; see http://www.erlang.org/doc/man/inet.html for the list of all errors.

error_unknown_data
------------------

Data received from the server during a thinktime (not for unparsed
protocol like XMPP). The session is aborted.

error_unknown_msg
-----------------

Unknown message received (see the log files for more information). The session is aborted.

error_unknown
-------------

Abnormal termination of a session, see log file for more information.

error_repeat_<REPEATNAME>
-------------------------

Error in a repeat loop (undefined dynamic variable usually).

error_send_<ERRORNAME>
----------------------

Error while sending data to the server, see
http://www.erlang.org/doc/man/inet.html for the list of all errors.

error_send
----------

Unexpected error while sending data to the server,
  see the logfiles for more information.

error_connect_<ERRORNAME>
-------------------------

Error while establishing a connection to the server.  See
http://www.erlang.org/doc/man/inet.html for the list of all errors.


error_no_online
---------------

XMPP: No online user available (usually for a chat message destinated
 to a online user)

error_no_offline
----------------

XMPP: No offline user available (usually for a chat message destinated
to a offline user)

error_no_free_userid
--------------------

For XMPP: all users Id are already used (``userid_max`` is too low ?)


error_next_session
------------------

A clients fails to gets its session parameter from the config_server;
the controller may be overloaded ?

error_mysql_<ERRNO>
-------------------

Error reported by the mysql server (see
http://dev.mysql.com/doc/refman/5.0/en/error-messages-server.html)

error_mysql_badpacket
---------------------

Bad packet received for mysql server while parsing data.

error_pgsql
-----------

Error reported by the postgresql server.


