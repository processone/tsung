===========
Errors list
===========


\item[error\_closed] Only for non persistent session (XMPP); the
  server unexpectedly closed the connection; the session is aborted.
\item[error\_inet\_<ERRORNAME>] Network error; see
  \url{http://www.erlang.org/doc/man/inet.html} for the list of all errors.
\item[error\_unknown\_data] Data received from the server during a
  thinktime (not for unparsed protocol like XMPP). The session is
  aborted.
\item[error\_unknown\_msg] Unknown message received (see the log
  files for more information). The session is aborted.
\item[error\_unknown] Abnormal termination of a session, see
  log file for more information.
\item[error\_repeat\_<REPEATNAME>] Error in a repeat loop (undefined
  dynamic variable usually).
\item[error\_send\_<ERRORNAME>] Error while sending data to the
  server, see  \url{http://www.erlang.org/doc/man/inet.html} for the list of all errors.
\item[error\_send] Unexpected error while sending data to the server,
  see the logfiles for more information.
\item[error\_connect\_<ERRORNAME>] Error while establishing a
  connection to the server.  See  \url{http://www.erlang.org/doc/man/inet.html} for the list of all errors.
\item[error\_no\_online jabber] XMPP: No online user available (usually for a
  chat message destinated to a online user)
\item[error\_no\_offline jabber] XMPP: No offline user available (usually for a
  chat message destinated to a offline user)
\item[error\_no\_free\_userid] For XMPP: all users Id are already used
  (\varname{userid\_max} is too low ?)
\item[error\_next\_session] A clients fails to gets its session
  parameter from the config\_server; the controller may be overloaded ?
\item[error\_mysql\_<ERRNO>] Error reported by the mysql server (see \url{http://dev.mysql.com/doc/refman/5.0/en/error-messages-server.html})
\item[error\_mysql\_badpacket] Bad packet received for mysql server while parsing data.
\item[error\_pgsql] Error reported by the postgresql server.


