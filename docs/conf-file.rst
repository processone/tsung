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
only implemented for HTTP: this will log all requests in a CSV file,
with the following data:

.. code-block:: text

   #date;pid;id;http method;host;URL;HTTP status;size;duration;match;error

Where:

======== =====================================================================================
field    description
======== =====================================================================================
date     timestamp at the end of the request (seconds since 1970-01-01 00:00:00 UTC)
pid      erlang process id
id       tsung user id
host     server hostname
url      URL (relative)
HTTP     status HTTP reponse status (200, 304, etc.)
size     reponse size (in bytes)
duration request duration (msec)
match    if a match is defined in the request: match|nomatch (last <match> if several are defined)
error    name of http error (or empty)
======== =====================================================================================

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
