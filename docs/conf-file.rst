.. _sec-file-structure-label:

File structure
==============

The default :index:`encoding` is utf-8.  You can use a different encoding, like in:

.. code-block:: xml

 <?xml version="1.0" encoding="ISO-8859-1"?>

Scenarios are enclosed into **tsung** tags:

.. code-block:: xml

   <?xml version="1.0"?>
   <!DOCTYPE tsung SYSTEM "/usr/share/tsung/tsung-1.0.dtd" [] >
   <tsung loglevel="info">
   ...
   </tsung>

.. index:: dumptraffic

If you add the attribute **dumptraffic="true"**, all the
traffic will be logged to a file.

.. warning::

   this will considerably slow down Tsung, so use with care. It is
   useful for debugging purpose. You can use the attribute
   **dumptraffic="light"** to dump only the first 44 bytes.

Since version **1.4.0**, you have also a specific logging per
protocol, using **dumptraffic="protocol"**. It's currently
only implemented for HTTP: this will log all requests in a CSV (RFC4180) file,
with the following data:

.. code-block:: text

   date,pid,id,start,connect,time_to_first_byte,duration,host,http_method,relative_url,http_status,response_size,transaction,match,error,tag

Where:

==================== ============================================================================================================
field                description
==================== ============================================================================================================
date                 timestamp of log entry
pid                  erlang process id
id                   tsung user id
start                timestamp when request was started
connect              duration in msec to establish the TCP connection, 0 if the connection was already established
time_to_first_byte   duration in msec it took after request was send, before first response byte was received
duration             duration in msec for the entire request to complete (connect + time to first byte + rest of transfer)
host                 server host name
http_method          HTTP Method/Verb (GET, POST, PUT, etc.)
relative_url         Relative URL
http_status          status HTTP response status (200, 304, etc.)
response_size        response size (in bytes)
transaction          name of the transaction (if any) this request was made in
match                if a match is defined in the request: match|nomatch (last <match> if several are defined)
error                name of http error (or empty)
tag                  tag name if the request was tagged; empty otherwise
==================== ============================================================================================================

Timestamps are always in epoch, seconds with fractions since 1970-01-01 00:00:00 UTC. Field values are unquoted unless they
contain a comma or quote (").

.. warning::

   In the general case (several Tsung clients used), the resulting
   file will not be sorted, so you may have to sort it before analyzing it.

For heavy load testing (tens of thousands requests per second), the
**protocol** logging may overload the controller. In this case, you can
use **protocol_local** instead. In this case, the log files will be
written on each slave locally. You will have to manually merged the
logs at the end of the test.

.. index:: loglevel

The **loglevel** can also have a great impact on performance:
For high load, **warning** is recommended.

Possible values are:

* emergency
* critical
* error
* warning
* notice *(default)*
* info
* debug


For REALLY verbose logging, recompile tsung with :command:`make debug`
and set **loglevel** to **debug**.
