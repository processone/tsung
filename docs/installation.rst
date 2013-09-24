============
Installation
============

This package has been tested on Linux, FreeBSD and Solaris. A port is
available on MacOS X. It should work on Erlang supported platforms
(Linux, Solaris, \*BSD, Win32 and MacOS-X).


Dependencies
============

* Erlang/OTP R13B and up (`download <http://www.erlang.org/download.html>`_). Erlang is now
  part of fedora and debian/ubuntu repositories.

* pgsql module made by Christian Sunesson (for the PostgreSQL plugin):
  sources available at http://jungerl.sourceforge.net/ . The module is
  included in the source and binary distribution of ``Tsung``. It
  is released under the EPL License.

* mysql module made by Magnus Ahltorp & Fredrik Thulin  (for the mysql plugin):
  sources available at
  http://www.stacken.kth.se/projekt/yxa/. The modified module is
  included in the source and binary distribution of ``Tsung``. It
  is released under the three-clause BSD License.

* eldap module  (for the LDAP plugin):
  sources available at http://jungerl.sourceforge.net/. The module is
  included in the source and binary distribution of ``Tsung``. It
  is released under the GPL License.

* mochiweb libs (for xpath parsing, optionally used for dynamic variables in the HTTP plugin):
  sources available at http://code.google.com/p/mochiweb/. The module
  is included in the source and binary distribution of ``Tsung``. It
  is released under the MIT License.
    
* gnuplot and perl5 (optional; for graphical output with
  *tsung_stats.pl* script).  The Template Toolkit is used for HTML
  reports (see http://template-toolkit.org/ )

* python and mathplotlib (optional; for graphical output with ``tsung-plotter``).

* for distributed tests, you need an ssh access to remote machines
  without password (use a RSA/DSA key without pass-phrase or
  ssh-agent) (rsh is also supported)

* bash


Compilation
===========

| ./configure
| make
| make install


If you want to download the development version, use git:

``git clone https://github.com/processone/tsung.git``, see also
 https://github.com/processone/tsung

You can also build packages with *make deb* (on debian and ubuntu) and
*make rpm* (on fedora, rhel, and other rpm based distribution)



Configuration
=============

The default configuration file is :file:`~/.tsung/tsung.xml` (there
are several sample files in :file:`/usr/share/doc/tsung/examples`).

Log files are saved in :file:`~/.tsung/log/`. A new sub-directory
is created for each test using the current date as name
(:file:`~/.tsung/log/20040217-0940` for ex.)


Running
=======

Two commands are installed in the directory ``$PREFIX/bin``:
``tsung`` and ``tsung-recorder``.
A man page is available for both commands.

.. literalinclude:: tsung-help.txt

A typical way of using tsung is to run: ``tsung -f myconfigfile.xml start``.

The command will print the current log directory created for the test, and wait until the test is
over.

Feedback
========

Use the `Tsung mailing list <https://lists.process-one.net/mailman/listinfo/tsung-users>`_ if you have
suggestions or questions about ``Tsung``. You can also use the
bug-tracker available at https://support.process-one.net/browse/TSUN.

You can also try the #tsung IRC channel on Freenode.
