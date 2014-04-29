Advanced Features
-----------------

Dynamic substitutions
^^^^^^^^^^^^^^^^^^^^^

Dynamic substitution are mark-up placed in element of the scenario.
For HTTP, this mark-up can be placed in basic authentication (www\_authenticate
tag: userid and passwd attributes), URL (to change GET parameter)
and POST content.

Those mark-up are of the form ``%%Module:Function%%``.
Substitutions are executed on a request-by-request basis, only if the
request tag has the attribute ``subst="true"``.

When a substitution is requested, the substitution mark-up is replaced by
the result of the call to the Erlang function:
``Module:Function({Pid, DynData})`` where ``Pid`` is the Erlang process
id of the current virtual user and DynData the list of all Dynamic
variables (**Warn: before version 1.1.0, the argument was just the
Pid!**).

Here is an example of use of substitution in a Tsung scenario:

.. code-block:: xml

   <session name="rec20040316-08:47" probability="100" type="ts_http">
     <request subst="true">
       <http url="/echo?symbol=%%symbol:new%%" method="GET"></http>
     </request>
   </session>

For the http plugin, and since version 1.5.1, you can use the special value
``subst='all_except_body'`` instead of ``'true'`` to skip the substitutions in
the body part of the HTTP response.

Here is the Erlang code of the module used for dynamic substitution:

.. code-block:: erlang

 -module(symbol).
 -export([new/1]).

 new({Pid, DynData}) ->
    case random:uniform(3) of
        1 -> "IBM";
        2 -> "MSFT";
        3 -> "RHAT"
    end.


Use :command:`erlc` to compiled the code, and put the resulting .beam
file in :file:`\$PREFIX/lib/erlang/lib/tsung-X.X.X/ebin/` on all client
machines.

As you can see, writing scenario with dynamic substitution is
simple. It can be even simpler using dynamic variables (see later).

.. index:: ts_user_server

If you want to set unique id, you can use the built-in function
**ts_user_server:get_unique_id**.

.. code-block:: xml

 <session name="rec20040316-08:47" probability="100" type="ts_http">
   <request subst="true">
     <http url="/echo?id=%%ts_user_server:get_unique_id%%" method="GET" />
   </request>
 </session>


Reading external file
^^^^^^^^^^^^^^^^^^^^^

**New in 1.0.3**: A new module ``ts_file_server`` is available. You
can use it to read external files. For example, if you need to read user
names and passwd from a CSV file, you can do it with it (currently,
you can read only a single file).


You have to add this in the XML configuration file:

.. code-block:: xml

 <option name="file_server"  value="/tmp/userlist.csv"></option>


**New in 1.2.2**: You can read several files, using the **id**
attribute to identify each file:

.. code-block:: xml

 <option name="file_server" value="/tmp/userlist.csv"></option>
 <option name="file_server" id='random' value="/tmp/randomnumbers.csv"></option>


Now you can build your own function to use it, for example, create a
file called :file:`readcsv.erl`:

.. code-block:: erlang

 -module(readcsv).
 -export([user/1]).

 user({Pid,DynVar})->
    {ok,Line} = ts_file_server:get_next_line(),
    [Username, Passwd] = string:tokens(Line,";"),
    "username=" ++ Username ++"&password=" ++ Passwd.


The output of the function will be a string ``username=USER&password=PASSWORD``

Then compile it with :command:`erlc readcsv.erl` and put
:file:`readcsv.beam` in :file:`$prefix/lib/erlang/lib/tsung-VERSION/ebin` directory (if the
file has an id set to ``random``, change the call to ``ts_file_server:get_next_line(random)``).

Then use something like this in your session:

.. code-block:: xml

  <request subst="true">
    </http>
  </request>


Two functions are available: ``ts_file_server:get_next_line``
and ``ts_file_server:get_random_line``. For the
``get_next_line`` function, when the end of file is reached, the
first line of the file will be the next line.

**New in 1.3.0**: you no longer have to create an external
function to parse a simple csv file: you can use ``setdynvars``
(see next section for detailed documentation):

.. code-block:: xml

 <setdynvars sourcetype="file" fileid="userlist.csv" delimiter=";" order="iter">
  <var name="username" />
  <var name="user_password" />
 </setdynvars>


This defines two dynamic variables **username** and
**user_password** filled with the next entry from the csv
file. Using the previous example, the request is now:

.. code-block:: xml

  <request subst="true">
    <http url='/login.cgi' version='1.0'
      contents='username=%%_username%%&amp;password=%%_user_password%%&amp;op=login'
    content_type='application/x-www-form-urlencoded' method='POST'>
    </http>
  </request>


Much simpler than the old method!

In case you have several arrival phases programmed and if you use file with
``order="iter"`` the position in the file will not be reset between different
arrival phase. You will not be returned to the first line when changing phase.

.. code-block:: xml

  <arrivalphase phase="1" duration="10" unit="minute">
    <users maxnumber="10" arrivalrate="100" unit="second" />
  </arrivalphase>
  <arrivalphase phase="2" duration="10" unit="minute">
    <users maxnumber="20" arrivalrate="100" unit="second"></users>
  </arrivalphase>


In this example phase 1 will read about 10 lines and phase 2 will read the next
20 lines.

.. TODO explain, that file servers are synchronized between tsung nodes in a distributed setup.

.. index:: dyn_variable
.. _sec-dynamic-variables-label:

Dynamic variables
^^^^^^^^^^^^^^^^^

In some cases, you may want to use a value given by the server in a
response later in the session, and this value is **dynamically
generated** by the server for each user. For this, you can use
``<dyn_variable>`` in the scenario

Let's take an example with HTTP. You can easily grab a value in a HTML
form like:

.. code-block:: html

 <form action="go.cgi" method="POST">
   <hidden name="random_num" value="42"></form>
 </form>

with:

.. code-block:: xml

 <request>
   <dyn_variable name="random_num"></dyn_variable>
   <http url="/testtsung.html" method="GET" version="1.0"></http>
 </request>


Now ``random_num`` will be set to 42 during the users session. Its
value will be replace in all mark-up of the form
``%%_random_num%%`` if and only if the ``request`` tag has the
attribute ``subst="true"``, like:

.. code-block:: xml

  <request subst="true">
    <http url="/go.cgi" version="1.0"
      contents="username=nic&amp;random_num=%%_random_num%%&amp;op=login"
      content_type="application/x-www-form-urlencoded" method="POST">
    </http>
  </request>


Regexp
""""""

If the dynamic value is not a form variable, you can set a regexp by
hand, for example to get the title of a HTML page: the regexp engine
uses the ``re`` module, a Perl like regular expressions module
for Erlang.

.. code-block:: xml

    <request>
      <dyn_variable name="mytitlevar"
                    re="&lt;title&gt;(.*)&lt;/title&gt;"/>
      <http url="/testtsung.html" method="GET" version="1.0"></http>
    </request>


Previously (before 1.4.0), Tsung uses the old ``regexp`` module
from Erlang. This is now deprecated. The syntax was:

.. code-block:: xml

    <request>
      <dyn_variable name="mytitlevar"
                    regexp="&lt;title&gt;\(.*\)&lt;/title&gt;"/>
      <http url="/testtsung.html" method="GET" version="1.0"></http>
    </request>

.. index:: xpath

XPath
"""""

A new way to analyze the server response has been introduced in the
release **1.3.0**. It is available only for the HTTP and XMPP plugin since it is
based on XML/HTML parsing. This feature uses the mochiweb library
and **only works with Erlang R12B and newer version**.

This give us some benefices:

* XPath is simple to write and to read, and match very well with
  HTML/XML pages

* The parser works on ``binaries()``, and doesn't create any
  ``string()``.

* The cost of parsing the HTML/XML and build the tree is amortized
  between all the dyn_variables defined for a given request


To utilize XPath expression, use a ``xpath`` attribute when
defining the ``dyn_variable``, instead of ``re``, like:

.. code-block:: xml

 <dyn_variable name="field1_value" xpath="//input[@name='field1']/@value"/>
 <dyn_variable name="title" xpath="/html/head/title/text()"/>


There is a bug in the XPath engine, result nodes from
"descendant-or-self" aren't returned in document order. This isn't a
problem for the most common cases.

However, queries like ``//img[1]/@src`` are not recommended,
as the order of the ``<img>`` elements returned from ``//img`` is
not the expected.

The order is respected for paths without "descendant-or-self" axis, so
this: ``/html/body/div[2]/img[3]/@src`` is interpreted as
expected and can be safely used.

It is possible to use XPath to get a list of elements from an html page,
allowing dynamic retrieval of objects. You can either create embedded
Erlang code to parse the list produced, or use foreach that was introduced
in release **1.4.0**.

For XMPP, you can get all the contacts in a dynamic variable:

.. code-block:: xml

 <request subst="true">
    <dyn_variable name="contactJids"
      xpath="//iq[@type='result']/query[@xmlns='jabber:iq:roster']//item[string-length(@wr:type)=0]/@jid" />
    <jabber type="iq:roster:get" ack="local"/>
 </request>


.. index:: jsonpath

.. _sec-jsonpath-label:

JSONPath
""""""""

Another way to analyze the server response has been introduced in the
release **1.3.2** when the server is sending JSON data. It is
only for the HTTP plugin. This feature uses the mochiweb library and
**only works with Erlang R13B and newer version**.

Tsung implements a (very) limited subset of JSONPath as defined here
http://goessner.net/articles/JsonPath/

To utilize jsonpath expression, use a **jsonpath** attribute when
defining the ``<dyn_variable>>``, instead of ``re``, like:

.. code-block:: xml

   <dyn_variable name="array3_value" jsonpath="field.array[3].value"/>


You can also use expressions ``Key=Val``, e.g.:

.. code-block:: xml

   <dyn_variable name="myvar" jsonpath="field.array[?name=bar].value"/>


PostgreSQL
""""""""""

.. versionadded:: 1.3.2

Since the  PostgreSQL protocol is binary, regexp are not useful to
parse the output of the server. Instead, a specific parsing can be
done to extract content from the server's response; to do this, use the
``pgsql_expr`` attribute. Use ``data_row[L][C]`` to
extract the  column C of the  line L of the data output. You can also use
the literal name of the column (ie. the field name of the
table). This example extract 3 dynamic variables from the server's
response:

First one, extract the 3rd column of the fourth row, then the ``mtime``
field from the second row, and then it extract some data of the
``row_description``.

.. code-block:: xml

 <request>
   <dyn_variable name="myvar" pgsql_expr="data_row[4][3]"/>
   <dyn_variable name="mtime" pgsql_expr="data_row[2].mtime"/>
   <dyn_variable name="row" pgsql_expr="row_description[1][3][1]"/>
   <pgsql type="sql">SELECT * from pgbench_history LIMIT 20;</pgsql>
 </request>


A row description looks like this::

  | =INFO REPORT==== 14-Apr-2010::11:03:22 ===
  |            ts_pgsql:(7:<0.102.0>) PGSQL: Pair={row_description,
  |                                                [{"tid",text,1,23,4,-1,16395},
  |                                                 {"bid",text,2,23,4,-1,16395},
  |                                                 {"aid",text,3,23,4,-1,16395},
  |                                                 {"delta",text,4,23,4,-1,16395},
  |                                                 {"mtime",text,5,1114,8,-1,16395},
  |                                                 {"filler",text,6,1042,-1,26,16395}]}


So in the example, the **row** variable equals "aid".

Decoding variables
""""""""""""""""""

It's possible to decode variable that contains html entities encoded,
this is done with **decode** attribute set to **html_entities**.

.. code-block:: xml

 <request>
   <dyn_variable name="mytitlevar"
                 re="&lt;title&gt;(.*)&lt;/title&gt;"
                 decode="html_entities"/>
   <http url="/testtsung.html" method="GET" version="1.0"></http>
 </request>

.. index:: setdynvars

set_dynvars
"""""""""""

**Since version 1.3.0**, more powerful dynamic variables are implemented.

You can set dynamic variables not only while parsing server data, but
you can build them using external files or generate them with a function
or generate random numbers/strings:

Several types of dynamic variables are implemented (``sourcetype`` attribute):

.. index:: callback

* Dynamic variables defined by calling an Erlang function:

  .. code-block:: xml

     <setdynvars sourcetype="erlang" callback="ts_user_server:get_unique_id">
        <var name="id1" />

.. index:: delimiter
.. index:: fileid
.. index:: iter

* Dynamic variables defined by parsing an external file:

  .. code-block:: xml

     <setdynvars sourcetype="file" fileid="userdb" delimiter=";" order="iter">
       <var name="user" />
       <var name="user_password" />
     </setdynvars>

  *delimiter* can be any string, and *order* can be
  **iter** or **random**

*  A dynamic variable can be a random number (uniform distribution)

   .. code-block:: xml

      <setdynvars sourcetype="random_number" start="3" end="32">
        <var name="rndint" />
      </setdynvars>

* A dynamic variable can be a random string

  .. code-block:: xml

     <setdynvars sourcetype="random_string" length="13">
        <var name="rndstring1" />
     </setdynvars>

* A dynamic variable can be a urandom string: this is much faster than
  the random string, but the string is not really random: the same set
  of characters is always used.

* A dynamic variable can be generated by dynamic evaluation of erlang code:

  .. code-block:: xml

     <setdynvars sourcetype="eval"
                 code="fun({Pid,DynVars})->
                           {ok,Val}=ts_dynvars:lookup(md5data,DynVars),
                           ts_digest:md5hex(Val) end.">
       <var name="md5sum" />
     </setdynvars>


  In this case, we use tsung function ``ts_dynvars:lookup`` to retrieve the
  dynamic variable named ``md5data``. This dyn\_variable ``md5data``
  can be set in any of the ways described in the Dynamic variables
  section :ref:`sec-dynamic-variables-label`.

* A dynamic variable can be generated by applying a JSONPath
  specification (see :ref:`sec-jsonpath-label`) to an existing dynamic
  variable:

  .. code-block:: xml

     <setdynvars sourcetype="jsonpath" from="notification" jsonpath="result[?state=OK].node">
       <var name="deployed" />
     </setdynvars>

* You can create dynamic variables to get the hostname and port of the current server

  .. code-block:: xml

    <setdynvars sourcetype="server">
      <var name="host" />
      <var name="port" />
    </setdynvars>


* You can define a dynamic variable as constant value to use it in
  a plugin (since version **1.5.0**)

  .. code-block:: xml

     <setdynvars sourcetype="value" value="foobar">
       <var name="constant" />
     </setdynvars>




A **setdynvars** can be defined anywhere in a session.


.. index:: match

Checking the server's response
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With the tag ``match`` in a ``<request>`` tag, you can check
the server's response against a given string, and do some actions
depending on the result. In any case, if it matches, this will
increment the ``match`` counter, if it does not match, the
``nomatch`` counter will be incremented.

For example, let's say you want to test a login page. If the login is
ok, the server will respond with ``Welcome !`` in the
HTML body, otherwise not. To check that:

.. code-block:: xml

 <request>
    <match do="continue" when="match">Welcome !</match>
    <http url="/login.php" version="1.0" method="POST"
          contents="username=nic&amp;user_password=sesame"
          content_type="application/x-www-form-urlencoded" >
 </request>


You can use a regexp instead of a simple string.

The list of available actions to do is:

* **continue**: do nothing, continue (only update match or nomatch counters)

* **log**: log the request id, userid, sessionid, name in a file (in :file:`match.log`)

* **abort**: abort the session

* **restart**: restart the session. The maximum number of
  restarts is 3 by default.

* **loop**: repeat the request, after 5 seconds. The maximum number of
  loops is 20 by default.

* **dump**: dump the content of the response in a file. The filename
  is :file:`match-<userid>-<sessionid>-<requestid>-<dumpid>.dump`


You can mixed several match tag in a single request:

.. code-block:: xml

   <request>
     <match do="loop" sleep_loop="5" max_loop="10" when="match">Retry</match>
     <match do="abort" when="match">Error</match>
     <http url='/index.php' method=GET'>
   </request>


You can also do the action on **nomatch** instead of **match**.

.. index:: skip_headers
.. index:: apply_to_content

If you want to skip the HTTP headers, and match only on the body, you
can use **skip_headers='http'**. Also, you can apply a
function to the content before matching; for example the following
example use both features to compute the md5sum on the body of a HTTP
response, and compares it to a given value:

.. code-block:: xml

   <match do='log' when='nomatch' skip_headers='http' apply_to_content='ts_digest:md5hex'>01441debe3d7cc65ba843eee1acff89d</match>
   <http url="/" method="GET" version="1.1"/>


You can also use dynamic variables, using the **subst** attribute:

.. code-block:: xml

   <match do='log' when='nomatch' subst='true' >%%_myvar%%</match>
   <http url="/" method="GET"/>


**Since 1.5.0**, it's now possible to add **name** attribute in **match** tag to name a record printed in match.log as follow:

.. code-block:: xml

   <match do='log' when='match' name='http_match_200ok'>200OK</match>
   <http url="/" method="GET" version="1.1"/>


Loops, If, Foreach
^^^^^^^^^^^^^^^^^^

**Since 1.3.0**, it's now possible to add conditional/unconditional loops in a session.

**Since 1.4.0**, it is possible to loop through a list of dynamic variables thanks to foreach.

.. index:: for

<for>
"""""

Repeat the enclosing actions a fixed number of times. A dynamic
variable is used as counter, so the current iteration could be used in
requests. List of attributes:

``from``
  Initial value
``to``
  Last value
``incr``
  Amount to increment in each iteration
``var``
  Name of the variable to hold the counter


.. code-block:: xml

 <for from="1" to="10" incr="1" var="counter">
   ...
   <request> <http url="/page?id=%%_counter%%"></http> </request>
   ...
 </for>

.. index:: repeat
.. index:: while
.. index:: until

<repeat>
""""""""

Repeat the enclosing action (while or until) some condition. This is
intended to be used together with ``<dyn_variable>`` declarations. List of
attributes:

``name``
  Name of the repeat

``max_repeat``
  Max number of loops (default value is 20)


The last element of repeat must be either ``<while>`` or ``<until>`` example:

.. code-block:: xml

 <repeat name="myloop" max_repeat="40">
   ...
   <request>
     <dyn_variable name="result" re="Result: (.*)"/>
     <http url="/random" method="GET" version="1.1"></http>
   </request>
   ...
   <until var="result" eq="5"/>
 </repeat>


**Since 1.3.1**, it's also possible to add if statements based on
dynamic variables:

.. index:: if

<if>
""""

.. code-block:: xml

 <if var="tsung_userid" eq="3">
   <request> <http url="/foo"/> </request>
   <request> <http url="/bar"/> </request>
 </if>


You can use ``eq`` or ``neq`` to check the variable.

**Since 1.5.1** you can also use the comparison operators ``gt``,
``gte``, ``lt`` and ``lte`` to do respectively ``greater than``,
``greater than or equal to``, ``less than`` and ``less than or equal to``.

If the dynamic variable is a list (output from XPath for example), you
can access to the n-th element of a list like this:

.. code-block:: xml

 <if var="myvar[1]" eq="3">

Here we compare the first element of the list to 3.

.. index:: foreach

<foreach>
"""""""""

Repeat the enclosing actions for all the elements contained in the list specified. The basic syntax is as follows:

.. code-block:: xml

 <foreach name="element" in="list">
   <request subst="true">
    <http url="%%_element%%" method="GET" version="1.1"/>
   </request>
 </foreach>


It is possible to limit the list of elements you're looping through, thanks to the use of the ``include`` or ``exclude`` attributes inside the foreach statement.

As an example, if you want to include only elements with a local path you can write:

.. code-block:: xml

 <foreach name="element" in="list" include="^/.*$">


If you want to exclude all the elements from a specific URI, you would write:

.. code-block:: xml

 <foreach name="element" in="list" exclude="http:\/\/.*\.tld\.com\/.*$">


You can combine this with a XPath query. For instance the following scenario will retrieve all the images specified on a web page:


.. code-block:: xml

 <request subst="true">
   <dyn_variable name="img_list" xpath="//img/@src"/>
   <http url="/mypage.html" method="GET" version="1.1"/>
 </request>
 <foreach name="img" in="img_list">
   <request subst="true">
     <http url="%%_img%%" method="GET" version="1.1"/>
   </request>
 </foreach>

Rate limiting
^^^^^^^^^^^^^

Since version **1.4.0**, rate limiting can be enabled, either globally
(see :ref:`sec-options-label`), or for each session separately.

For example, to limit the rate to 64KB/sec for a given session:

.. code-block:: xml

  <session name="http-example" probability="70" type="ts_http">
    <set_option name="rate_limit" value="64" />
    ...
  </session>


Only the incoming traffic is rate limited currently.

.. index:: tag

Requests exclusion
^^^^^^^^^^^^^^^^^^

.. versionadded:: 1.5.1

It is possible to exclude some request for a special run. To do this
you have to tag them and use the option ``-x`` when launching the run.

For example, to exclude the GET of foo.png, add a ``tag`` to the
respective request:

.. code-block:: xml

   <request>
     <http url="/" method="GET"></http>
   </request>
   <request tag="image">
     <http url="/foo.png" method="GET"></http>
   </request>

Then launch the run with::

   tsung -f SCENARIO.xml -x image start

Only the GET to ``/`` will be performed.

Note that request tags also get logged on **dumptraffic="protocol"** (see :ref:`sec-file-structure-label`) 

Client certificate
^^^^^^^^^^^^^^^^^^
.. versionadded:: 1.5.1

It is possible to use a client certificate for ssl authentication. You
can use dynamic variables to set some parameters of the certificate
(and the key password is optional).

.. code-block:: xml

  <session name="https-with-cert" probability="70" type="ts_http">

    <set_option name="certificate">
      <certificate cacertfile="/etc/ssl/ca.pem"
                   keyfile="%%_keyfile%%" keypass="%%_keypass%%" certfile="/home/nobody/.tsung/client.pem"/>
    </set_option>
