
.. index:: proxy
.. index:: tsung-recorder
.. _tsung-recorder:

========================
Using the proxy recorder
========================

The recorder has three plugins: for HTTP, WebDAV and for PostgreSQL.

To start it, run :command:`tsung-recorder -p <PLUGIN> start`, where **PLUGIN** can be
*http*, *webdav* or *pgsql* for PostgreSQL. The default plugin is **http**.


The proxy is listening to port **8090**. You can change the port with
:option:`-L portnumber`.

To stop it, use :command:`tsung-recorder stop`.

The recorded session is created as
:file:`~/.tsung/tsung_recorderYYYMMDD-HH:MM.xml`; if it doesn't work,
take a look at :file:`~/.tsung/log/tsung.log-tsung_recorder@hostname`

.. index:: record_tag

During the recording, you can add custom tag in the XML file, this can
be useful to set transactions or comments:
:command:`tsung-recorder record_tag "<transaction name='login'>''`

Once a session has been created, you can insert it in your main configuration
file, either by editing by hand the file, or by using an ENTITY
declaration, like:

.. code-block:: xml

   <!DOCTYPE tsung SYSTEM "/usr/share/tsung/tsung-1.0.dtd" [
     <!ENTITY mysession1 SYSTEM "/home/nniclausse/.tsung/tsung_recorder20051217-13:11.xml">
   ]>
   ...
   <sessions>
     &mysession1;
   </sessions>


PostgreSQL
==========

For PostgreSQL, the proxy will connect to the server at IP 127.0.0.1
and port 5432. Use **-I serverIP** to change the IP and
**-P portnumber** to change the port.

HTTP and WEBDAV
===============

For HTTPS recording, use **http://-** instead of
**https://** in your browser

**New in 1.2.2**: For HTTP, you can configure the recorder to use a parent proxy (but this will not work for https). Add the :option:`-u`
option to enable parent proxy, and use **-I serverIP** to set the IP and **-P portnumber** to set the port of the parent.
