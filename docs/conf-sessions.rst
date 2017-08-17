.. index:: session
.. _sessions-label:

========
Sessions
========

Sessions define the content of the scenario itself. They describe
the requests to execute.

Each session has a given probability. This is used to decide which
session a new user will execute. The sum of all session's
probabilities must be 100.

**Since Tsung 1.5.0**, you can use weights instead of
probabilities. In the following example, there will be twice as many
sessions of type s1 than s2.

.. code-block:: xml

  <session name="s1" weight="2" type="ts_http">
  <session name="s2" weight="1" type="ts_http">


A transaction is just a way to have customized statistics. Say if you
want to know the response time of the login page of your website, you
just have to put all the requests of this page (HTML + embedded
pictures) within a transaction. In the example above, the transaction
called ``index_request`` will gives you in the
statistics/reports the mean response time to get
``index.en.html + header.gif``. Be warn that If you have a
thinktime inside the transaction, the thinktime will be part of the
response time.

.. index:: thinktimes

Thinktimes
^^^^^^^^^^

You can set static or random thinktimes to separate requests. By
default, a random thinktime will be a exponential distribution with
mean equals to ``value``.

.. code-block:: xml

 <thinktime value="20" random="true"></thinktime>


In this case, the thinktime will be an exponential distribution with a
mean equals to 20 seconds.

**Since version 1.3.0**, you can also use a range
``[min:max]`` instead of a mean for random thinktimes (the
distribution will be uniform in the interval):

.. code-block:: xml

 <thinktime min="2" max="10" random="true"></thinktime>


**Since version 1.4.0**, you can use a dynamic variable to set
the thinktime value:

.. code-block:: xml

 <thinktime value="%%_rndthink%%" random="true"></thinktime>

You can also synchronize all users using the ``wait_global`` value:

.. code-block:: xml

 <thinktime value='wait_global'>

which means: wait for all (N) users to be connected and waiting for
the global lock (the value can be set using the option ``<option
name="global_number" value ="XXX"/>`` and by setting `maxnumber=N` in
``<arrivalphase>``).

**Since version 1.6.0**, you can wait for a 'bidi' ack. If your protocol is bidirectional (e.g. xmpp, websocket, ...), you can wait until the server sends some data, and the code that handle this data exits the ``think`` state.

.. code-block:: xml

   <thinktime value="wait_bidi"></thinktime> -

HTTP
^^^^


This example shows several features of the HTTP protocol support in
Tsung: GET and POST request, basic authentication, transaction for
statistics definition, conditional request (IF MODIFIED SINCE):


.. code-block:: xml

 <sessions>
  <session name="http-example" probability="70" type="ts_http">

    <request> <http url="/" method="GET" version="1.1">
                    </http> </request>
    <request> <http url="/images/logo.gif"
               method="GET" version="1.1"
               if_modified_since="Fri, 14 Nov 2003 02:43:31 GMT">
              </http></request>

    <thinktime value="20" random="true"></thinktime>

    <transaction name="index_request">
     <request><http url="/index.en.html"
                          method="GET" version="1.1" >
              </http> </request>
     <request><http url="/images/header.gif"
                          method="GET" version="1.1">
              </http> </request>
    </transaction>

    <thinktime value="60" random="true"></thinktime>
    <request>
      <http url="/" method="POST" version="1.1"
               contents="bla=blu">
      </http> </request>
    <request>
       <http url="/bla" method="POST" version="1.1"
             contents="bla=blu&amp;name=glop">
       <www_authenticate userid="Aladdin"
                         passwd="open sesame"/></http>
    </request>
  </session>

  <session name="backoffice" probability="30" >
   <!--  -->
  </session>
 </sessions>


If you use an absolute URL, the server used in the URL will override
the one specified in the ``<server>`` section. The following relative
requests in the session will also use this new server value (until a
new absolute URL is set).

**New in 1.2.2**: You can add any HTTP header now, as in:

.. code-block:: xml

 <request>
   <http url="/bla" method="POST" contents="bla=blu&amp;name=glop">
     <www_authenticate userid="Aladdin" passwd="open sesame"/>
     <http_header name="Cache-Control" value="no-cache"/>
     <http_header name="Referer" value="http://www.w3.org/"/>
   </http>
 </request>


**New in 1.3.0**: You can also read the content of a POST or PUT
request from an external file:

.. code-block:: xml

 <http url="mypage" method="POST" contents_from_file="/tmp/myfile" />


Since **1.3.1**, you can also manually set a cookie, though the
cookie is not persistent: you must add it in every ``<requests>``:

.. code-block:: xml

 <http url="/">
   <add_cookie key="foo" value="bar"/>
   <add_cookie key="id"  value="123"/>
 </http>


Authentication
""""""""""""""

Until Tsung 1.5.0, only Basic authentication was implemented. You can
now use Digest Authentication and OAuth 1.0.

To use Digest authentication:

.. code-block:: xml

 <!-- 1. First request return 401. We use dynvars to fetch nonce and realm -->
 <request>
   <dyn_variable name="nonce" header="www-authenticate/nonce"/>
   <dyn_variable name="realm" header="www-authenticate/realm"/>
   <http url="/digest" method="GET" version="1.1"/>
 </request>

  <!--
  2. This request will be authenticated. Type="digest" is important.
  We use the nonce and realm values returned from the previous
  If the webserver returns the nextnonce we set it to the nonce dynvar
  for use with the next request.
  Else it stays set to the old value
  -->
  <request subst="true">
    <dyn_variable name="nonce" header="authentication-info/nextnonce"/>
    <http url="/digest" method="GET" version="1.1">
      <www_authenticate userid="user" passwd="passwd" type="digest" realm="%%_realm%%" nonce="%%_nonce%%"/>
    </http>
  </request>


To use OAuth authentication:

.. code-block:: xml

 <!-- Getting a Request Token -->

   <request>
     <dyn_variable name="access_token" re="oauth_token=([^&amp;]*)"/>
       <dyn_variable name="access_token_secret" re="oauth_token_secret=([^&amp;]*)" />
       <http url="/oauth/example/request_token.php" method="POST" version="1.1" contents="empty">
         <oauth consumer_key="key" consumer_secret="secret"  method="HMAC-SHA1"/>
       </http>
   </request>

   <!-- Getting an Access Token -->

   <request subst='true'>
        <dyn_variable name="access_token" re="oauth_token=([^&amp;]*)"/>
        <dyn_variable name="access_token_secret" re="oauth_token_secret=([^&amp;]*)"/>
          <http url="/oauth/example/access_token.php" method="POST" version="1.1" contents="empty">
          <oauth consumer_key="key" consumer_secret="secret"  method="HMAC-SHA1" access_token="%%_access_token%%" access_token_secret="%%_access_token_secret%%"/>
        </http>
      </request>

      <!-- Making Authenticated Calls -->

      <request subst="true">
        <http url="/oauth/example/echo_api.php" method="GET" version="1.1">
         <oauth consumer_key="key" consumer_secret="secret" access_token="%%_access_token%%" access_token_secret="%%_access_token_secret%%"/>
        </http>
      </request>


.. _sec-session-jabber-label:

Jabber/XMPP
^^^^^^^^^^^

.. index:: jabber

Here is an example of a session definition for the Jabber/XMPP protocol:

.. code-block:: xml

   <sessions>
     <session probability="70" name="jabber-example" type="ts_jabber">

       <request> <jabber type="connect" ack="local" /> </request>

       <thinktime value="2"></thinktime>

       <transaction name="authenticate">
         <request> <jabber type="auth_get" ack="local"></jabber> </request>
         <request> <jabber type="auth_set_plain" ack="local"></jabber> </request>
       </transaction>

       <request> <jabber type="presence:initial" ack="no_ack"/> </request>

       <thinktime value="30"></thinktime>

       <transaction name="online">
         <request> <jabber type="chat" ack="no_ack" size="16" destination="online"/></request>
       </transaction>

       <thinktime value="30"></thinktime>

       <transaction name="offline">
         <request> <jabber type="chat" ack="no_ack" size="56" destination="offline"/><request>
       </transaction>

       <thinktime value="30"></thinktime>

       <transaction name="close">
         <request> <jabber type="close" ack="local"> </jabber></request>
       </transaction>
     </session>
   </sessions>

Message stamping
""""""""""""""""

It is possible to stamp chat message by setting ``stamped`` attribute of
``<jabber>`` element inside request to ``true``. The stamp will include current
timestamp and ID of the sender node. If the recipient will recognize the node ID,
it will compare the timestamp inside message with the current one. The difference
will be reported as ``xmpp_msg_latency`` metric (in milliseconds).
The aim of node ID comparison is to avoid slight inconsistencies
of timestamps on different Tsung nodes.

Only a fraction of requests will hit the same node they originated from,
but with request rate high enough this fraction should be sufficient.

``stamped`` is allowed only with ``size`` attribute. ``data`` will cause
``stamped`` to be ignored. There is a minimal length of the stamp,
roughly 30 bytes. When ``size`` is greater than stamp length, random
padding will be added to the stamp. If the stamp length is higher than
``size``, then only stamp will be used as messagecontent, effectively
exceeding specified length.

StartTLS
""""""""

To secure a stream with STARTTLS, use:

.. code-block:: xml

 <jabber type="starttls" ack="bidi_ack" />

Client certificate is implemented since **1.5.1**, for example, you can
use dynamic variables like this:

.. code-block:: xml

 <jabber type="starttls" ack="bidi_ack"
            cacertfile="%%_cacert%%"
            certfile="%%_certfile%%"
            keyfile="%%_keyfile%%" />

Roster
""""""

What you can do with rosters using Tsung:

You can

#. Add a new contact to their roster
   - The new contact is added to the ``Tsung Group`` group, and their name matches their JID

#. Send a ``subscribe`` presence notification to the new contact's JID
   - This results in a *pending* subscription

#. Rename a roster contact
   This changes the previously added contact's name from the default JID, to ``Tsung Testuser``

#. Delete the previously added contact.


Note that when you add a new contact, the contact JID is stored and
used for the operations that follow. It is recommended that for each
session which is configured to perform these operations, only do so
once. In other words, you would NOT want to ADD more than one new
contact per session. If you want to alter the rate that these roster
functions are used during your test, it is best to use the session
'probability' factor to shape this.


The nice thing about this is that when you test run is complete, your
roster tables should look the same as before you started the test. So,
if you set it up properly, you can have pre-loaded roster entries
before the test, and then use these methods to dynamically add,
modify, and remove roster entries during the test as well.


Example roster modification setup:

.. code-block:: xml

 <session probability="100" name="jabber-rostermod" type="ts_jabber">

    <!-- connect, authenticate, roster 'get', etc... -->

    <transaction name="rosteradd">
      <request>
        <jabber type="iq:roster:add" ack="no_ack" destination="online"></jabber>
      </request>
      <request>
        <jabber type="presence:subscribe" ack="no_ack"/>
      </request>
    </transaction>

    <!-- ... -->

    <transaction name="rosterrename">
      <request> <jabber type="iq:roster:rename" ack="no_ack"></jabber> </request>
    </transaction>

    <!-- ... -->

    <transaction name="rosterdelete">
      <request> <jabber type="iq:roster:remove" ack="no_ack"></jabber> </request>
    </transaction>

    <!-- remainder of session... -->

  </session>


See also :ref:`bidi-presence-label` for automatic handling of subscribing requests.

.. index:: sasl plain

SASL Plain
""""""""""

SASL Plain authentication example:

.. code-block:: xml

 <session probability="100" name="sasl" type="ts_jabber">

    <request> <jabber type="connect" ack="local"></jabber> </request>

    <thinktime value="10"></thinktime>

    <transaction name="authenticate">
     <request>
       <jabber type="auth_sasl" ack="local"></jabber></request>

     <request>
       <jabber type="connect" ack="local"></jabber> </request>

    <request>
       <jabber type="auth_sasl_bind" ack="local" ></jabber></request>
    <request>
       <jabber type="auth_sasl_session" ack="local" ></jabber></request>

    </transaction>


SASL Anonymous
""""""""""""""

SASL Anonymous authentication example:

.. code-block:: xml

  <session probability="100" name="sasl" type="ts_jabber">

    <request> <jabber type="connect" ack="local"></jabber> </request>

    <thinktime value="10"></thinktime>

    <transaction name="authenticate">
     <request>
       <jabber type="auth_sasl_anonymous" ack="local"></jabber></request>

     <request>
       <jabber type="connect" ack="local"></jabber> </request>

    <request>
       <jabber type="auth_sasl_bind" ack="local" ></jabber></request>
    <request>
       <jabber type="auth_sasl_session" ack="local" ></jabber></request>

    </transaction>



Presence
""""""""


* **type** can be either ``presence:broadcast`` or ``presence:directed``.
* **show** value must be either ``away``, ``chat``, ``dnd``, or ``xa``.
* **status** value can be any text.


For more info, see section 2.2 of :RFC:`3921`.

If you omit the **show** or **status** attributes, they default to **chat** and **Available** respectively.

Example of broadcast presence (broadcast to members of your roster):

.. code-block:: xml

    <request>
      <jabber type="presence:broadcast" show="away" status="Be right back..." ack="no_ack"/>
    </request>

    <thinktime value="5"></thinktime>

    <request>
      <jabber type="presence:broadcast" show="chat" status="Available
      to chat" ack="no_ack"/>
    </request>

    <thinktime value="5"></thinktime>

    <request>
      <jabber type="presence:broadcast" show="dnd" status="Don't bother me!" ack="no_ack"/>
    </request>
    <thinktime value="5"></thinktime>

    <request>
     <jabber type="presence:broadcast" show="xa" status="I may never come back..."
      ack="no_ack"/>
    </request>
    <thinktime value="5"></thinktime>

    <request> <jabber type="presence:broadcast" ack="no_ack"/> </request>
    <thinktime value="5"></thinktime>


Example of directed presence (sent to random ``online`` users):

.. code-block:: xml

    <request>
      <jabber type="presence:directed" show="away" status="Be right back..." ack="no_ack"/>
    </request>
    <thinktime value="5"></thinktime>

    <request>
      <jabber type="presence:directed" show="chat" status="Available to chat" ack="no_ack"/>
    </request>
    <thinktime value="5"></thinktime>

    <request>
      <jabber type="presence:directed" show="dnd" status="Don't bother me!" ack="no_ack"/>
    </request>
    <thinktime value="5"></thinktime>

    <request>
      <jabber type="presence:directed" show="xa" status="I may never come back..."
        ack="no_ack"/>
     </request>
    <thinktime value="5"></thinktime>

    <request>
      <jabber type="presence:directed" ack="no_ack"/>
    </request>
    <thinktime value="5"></thinktime>


MUC
"""


Tsung supports three MUC operations:

* Join a room (attribute ``type='muc:join'``)
* Send a message to a room (attribute ``type='muc:chat'``)
* Change nickname (attribute ``type='muc:nick'``)
* Exit a room (attribute ``type='muc:exit'``)


Here's an example:

.. code-block:: xml

 <!-- First, choose an random room and random nickname: -->
 <setdynvars sourcetype="random_number" start="1" end="100">
   <var name="room"/>
 </setdynvars>
 <setdynvars sourcetype="random_string" length="10">
   <var name="nick1"/>
 </setdynvars>

 <request subst="true">
   <jabber type='muc:join' ack="local" room="room%%_room%%" nick="%%_nick1%%"/>
 </request>

 <!-- use a for loop to send several messages to the room -->
 <for from="1" to="6" var="i">
  <thinktime value="30"/>
  <request subst="true">
    <jabber type="muc:chat" ack="no_ack" size="16" room="room%%_room%%"/>
  </request>
 </for>

 <!-- change nickname-->
 <thinktime value="2"/>
 <setdynvars sourcetype="random_string" length="10">
  <var name="nick2"/>
 </setdynvars>

 <request subst="true">
  <jabber type="muc:nick" room="room%%_room%%" nick="%%_nick2%%"
          ack="no_ack"/>
 </request>


MUC support is available since version **1.3.1**.

PubSub
""""""

Experimental support for PubSub is available in version **1.3.1**

You can read the following entry: https://support.process-one.net/browse/TSUN-115

VHost
"""""

VHost support is available since version **1.3.2**

Tsung is able to bench multiple vhost instances by choosing a
vhost XMPP name from a list at connection time in the scenario.

The vhost list is read from a file:

.. code-block:: xml

 <options>
 ...
 <option name="file_server" value="domains.csv" id="vhostfileId"></option>
 ...
 <option type="ts_jabber" name="vhost_file" value="vhostfileId"></option>
 ...
 </options>


When each client starts a session, it chooses randomly a domain (each domain has the
same probability).

.. _sec-read-user-jabber-csv-label:

Reading usernames and password from a CSV file
""""""""""""""""""""""""""""""""""""""""""""""

Since version 1.4.0, you can now use a CSV file to store the usernames
and password.

Configure the CSV file:

.. code-block:: xml

 <options>
  <option name="file_server" id='userdb' value="/home/foo/.tsung/users.csv"/>
 </options>


And then you have to defined two variables of type ``file``,
and the first jabber request (``connect``) must include a
``xmpp_authenticate`` tag:

.. code-block:: xml

 <session probability="100" name="jabber-example" type="ts_jabber">

  <setdynvars sourcetype="file" fileid="userdb" delimiter=";" order="iter">
    <var name="username" />
    <var name="password" />
  </setdynvars>

    <request subst='true'>
      <jabber type="connect" ack="no_ack">
         <xmpp_authenticate username="%%_username%%" passwd="%%_password%%"/>
      </jabber>
    </request>

   <thinktime value="2"></thinktime>

   <transaction name="authenticate">

   <request>
     <jabber type="auth_get" ack="local"> </jabber>
   </request>
   <request>
     <jabber type="auth_set_plain" ack="local"></jabber>
   </request>

  </transaction>
  ...
 </session>


Moreover (since **1.5.0**), when using chat messages to random or offline users, you
should disable the default users (not from CSV) by setting
``userid_max`` to ``0`` and by setting the fileid for
offline and random users (also used for pubsub):

.. code-block:: xml

 <options>
  <option type="ts_jabber" name="userid_max" value="0" />
  <option type="ts_jabber" name="random_from_fileid" value='userdb'/>
  <option type="ts_jabber" name="offline_from_fileid" value='userdb'/>
  <option type="ts_jabber" name="fileid_delimiter" value=";"/>
 </options>


The username (resp. passwd) should be the first (resp. second) entry in the each CSV line (the
delimiter is by default ``";"`` and can be overriden).

raw XML
"""""""

You can send raw XML data to the server using the ``raw`` type:

.. code-block:: xml

 <jabber type="raw" ack="no_ack" data="&lt;stream&gt;foo&lt;/stream&gt;"></jabber>


Beware: you must encode XML characters like ``<`` , ``>``, ``&``, etc.

resource
""""""""

By default, the XMPP resource is set to ``tsung``. Since
version 1.5.0, you can override this (in all ``auth_*`` and
``register`` requests) using the ``resource`` attribute.

PostgreSQL
^^^^^^^^^^

For PostgreSQL, 4 types of requests are available:

* connect (to a given database with a given username)
* authenticate (with password or not)
* sql (basic protocol)
* close


In addition, the following parts of the extended protocol is supported:

* copy, copydone and copyfail
* parse, bind, execute, describe
* sync, flush


This example shows most of the features of a PostgreSQL session:

.. code-block:: xml

  <session probability="100" name="pgsql-example" type="ts_pgsql">
    <transaction name="connection">
      <request>
        <pgsql type="connect" database="bench" username="bench" />
      </request>
    </transaction>

    <request><pgsql type="authenticate" password="sesame"/></request>

    <thinktime value="12"/>

    <request><pgsql type="sql">SELECT * from accounts;</pgsql></request>

    <thinktime value="20"/>

    <request><pgsql type="sql">SELECT * from users;</pgsql></request>

    <request><pgsql type='sql'><![CDATA[SELECT n.nspname as "Schema",
  c.relname as "Name",
  CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'i'
  THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN '%_toto_% END as "Type",
  u.usename as "Owner"
 FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_user u ON u.usesysid = c.relowner
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
 WHERE c.relkind IN ('r','v','S','')
      AND n.nspname NOT IN ('pg_catalog', 'pg_toast')
      AND pg_catalog.pg_table_is_visible(c.oid)
 ORDER BY 1,2;]]></pgsql></request>

    <request><pgsql type="close"></pgsql></request>

  </session>


Example with the extended protocol:

.. code-block:: xml

 <request><pgsql type='parse' name_prepared='P0_7'><![CDATA[BEGIN;]]></pgsql></request>
 <request><pgsql type='sync'/></request>
 <request><pgsql type='parse' name_prepared='P0_8'><![CDATA[UPDATE pgbench_accounts
  SET abalance = abalance + $1 WHERE aid = $2;]]></pgsql></request>
 <request><pgsql type='sync'/></request>
 <request><pgsql type='parse' name_prepared='P0_9'><![CDATA[SELECT
  abalance FROM pgbench_accounts
  WHERE aid = $1;]]></pgsql></request>
 <request><pgsql type='sync'/></request>
 <request><pgsql type='parse' name_prepared='P0_10'><![CDATA[UPDATE pgbench_tellers
   SET tbalance = tbalance + $1 WHERE tid = $2;]]></pgsql></request>
 <request><pgsql type='sync'/></request>
 <request><pgsql type='parse' name_prepared='P0_11'><![CDATA[UPDATE pgbench_branches
   SET bbalance = bbalance + $1 WHERE bid = $2;]]></pgsql></request>
 <request><pgsql type='sync'/></request>
 <request><pgsql type='parse' name_prepared='P0_12'><![CDATA[INSERT
   INTO pgbench_history (tid, bid, aid, delta, mtime)
   VALUES ($1, $2, $3, $4, CURRENT_TIMESTAMP);]]></pgsql></request>
 <request><pgsql type='sync'/></request>
 <request><pgsql type='parse' name_prepared='P0_13'><![CDATA[END;]]></pgsql></request>
 <request><pgsql type='sync'/></request>
 <request><pgsql type='bind' name_prepared='P0_7' formats='none' formats_results='text' /></request>
 <request><pgsql type='describe' name_portal=''/></request>
 <request><pgsql type='execute'/></request>
 <request><pgsql type='sync'/></request>
 <request><pgsql type='bind' name_portal='' name_prepared='P0_8'
   formats='none' formats_results='text'
   parameters='2924,37801'/></request>


.. _session-mysql-label:

MySQL
^^^^^


For MySQL, 4 types of requests are available (same as PostgreSQL):

* connect (to a given database with a given username)
* authenticate (with password or not)
* sql
* close


This example shows most of the features of a MySQL session:

.. code-block:: xml

 <session probability="100" name="mysql-example" type="ts_mysql">
 <request>
  <mysql type="connect" />
 </request>
 <request>
  <mysql type="authenticate" database="test" username="test" password="test" />
 </request>
 <request>
  <mysql type="sql">SHOW TABLES</mysql>
 </request>
 <request>
  <mysql type="sql">SELECT * FROM mytable</mysql>
 </request>
 <request>
  <mysql type="close" />
 </request>
 </session>


Websocket
^^^^^^^^^
.. _sec-session-websocket-label:

For Websocket, 3 types of requests are available:

* connect (to a given path)
* message (send message to server, add a attribute 'ack' to specify whether
  Tsung should wait for a response)
* close


Example with Websocket as a session type:

.. code-block:: xml

 <session probability="100" name="websocket-example" type="ts_websocket">
   <request subst="true">
     <websocket type="connect" path="/path/to/ws"></websocket>
   </request>
   <request>
     <dyn_variable name="uid" jsonpath="uid"/>
     <!-- send data with text frame, default binary-->
     <websocket type="message" frame="text">{"user":"user", "password":"password"}</websocket>
   </request>

   <request subst="true">
     <match do="log" when="nomatch">ok</match>
     <websocket type="message">{"uid":"%%_uid%%", "data":"data"}</websocket>
   </request>

   <request>
     <websocket type="message" ack="no_ack">{"key":"value"}</websocket>
   </request>

   <request>
     <websocket type="close"></websocket>
   </request>
 </session>

You can do substitution on attribute 'path' and message content, match a
response or define dynamic variables based on the response message.

You can also set the subprotocols in a connect message:

.. code-block:: xml

  <websocket type="connect" path="/path/to/ws" subprotocols="chat"></websocket>

If you use ``change_type`` to start a websocket, don't forget to set
``bidi="true"``, like this:

.. code-block:: xml

 <change_type new_type="ts_websocket" host="127.0.0.1" port="8080" server_type="tcp" restore="true" store="true" bidi="true"/>i

When connecting to a websocket server you can set the ``origin``, which can be
checked by a websocket as a security measure, see
https://tools.ietf.org/html/rfc6455#section-10.2 for more details.
If not set this defaults to the ``host`` value.

.. code-block:: xml

  <websocket type="connect" origin="https://example.com"></websocket>

AMQP
^^^^^^^^^
.. _sec-session-amqp-label:

For AMQP, it supports publish and consume messages on multiple channel,
Available request types:

* connection.open (to a given vhost)
* connection.close
* channel.open (with specified and valid channel id)
* channel.close (with specified and valid channel id)
* confirm.select (on specified and already opened channel)
* basic.qos (on specified and already opened channel, only supports
  attribute 'prefetch_count')
* basic.publish (with channel id, exchange name, routing_key and the payload
* basic.consume (with channel id, queue name)
* waitForConfirms (with timeout for confirmations from the server)
* waitForMessages (with timeout for messages delivered to the client)

Example with AMQP as a session type:

.. code-block:: xml

    <session probability="100" name="amqp-example" type="ts_amqp" bidi="true">
        <request>
            <amqp type="connection.open" vhost="/"></amqp>
        </request>

        <!--  open channel, channel id is from 1 to 10 -->
        <for from="1" to="10" incr="1" var="loops">
            <request>
                <amqp type="channel.open"></amqp>
            </request>
        </for>

        <!-- ignore this request if you don't need publisher confirm -->
        <for from="1" to="10" incr="1" var="loops">
            <request subst="true">
                <amqp type="confirm.select" channel="%%_loops%%"></amqp>
            </request>
        </for>

        <for from="1" to="10" incr="1" var="loops">
            <for from="1" to="100" incr="1" var="counter">
                <transaction name="publish">
                    <!-- specify payload_size to have tsung generate a payload for you -->
                    <request subst="true">
                        <amqp type="basic.publish" channel="%%_loops%%" exchange="test_exchange"
                        routing_key="test_queue" persistent="true" payload_size="100"></amqp>
                    </request>
            <!-- substitutions are supported on the payload. Payload will override payload_size. -->
                    <request subst="true">
                        <amqp type="basic.publish" channel="%%_loops%%" exchange="test_exchange"
                        routing_key="test_queue" persistent="true" payload="Test Payload"></amqp>
                    </request>
                </transaction>
            </for>

            <!-- if publish with confirm, add a waitForConfirms request as you need: timeout=1s -->
            <request>
                <amqp type="waitForConfirms" timeout="1"></amqp>
            </request>
        </for>

        <for from="1" to="10" incr="1" var="loops">
            <request subst="true">
                <amqp type="basic.consume" channel="%%_loops%%" queue="test_queue" ack="true"></amqp>
            </request>
        </for>

        <!-- wait to receive messages from the server: timeout=180s -->
        <request>
            <amqp type="waitForMessages" timeout="180"></amqp>
        </request>

        <for from="1" to="10" incr="1" var="loops">
            <request subst="true">
                <amqp type="channel.close" channel="%%_loops%%"></amqp>
            </request>
        </for>

        <request>
            <amqp type="connection.close"></amqp>
        </request>
    </session>

MQTT
^^^^^^^^^
.. _sec-session-mqtt-label:

It supports publish messages, subscribe and unsubscribe topics,
Available request types:

* connect (with options like clean_start, will_topic, username, password, etc.)
* disconnect
* publish (with topic name, qos level and retain flag)
* subscribe (with topic name and qos level)
* unsubscribe (with topic name)
* waitForMessages (with timeout for messages published from server to the
  client)

Example with MQTT as a session type:

.. code-block:: xml

    <session name="mqtt-example" probability="100" type="ts_mqtt">
        <request>
            <mqtt type="connect" clean_start="true" keepalive="10" will_topic="will_topic" will_qos="0" will_msg="will_msg" will_retain="false"></mqtt>
        </request>

        <for from="1" to="10" incr="1" var="loops">
            <request subst="true">
                <mqtt type="publish" topic="test_topic" qos="1" retained="true">test_message</mqtt>
            </request>
        </for>

        <request subst="true">
            <mqtt type="subscribe" topic="test_topic" qos="1"></mqtt>
        </request>

        <request>
            <!-- wait for 60s -->
            <mqtt type="waitForMessages" timeout="60"></mqtt>
        </request>

        <request subst="true">
            <mqtt type="unsubscribe" topic="test_topic"></mqtt>
        </request>

        <request>
            <mqtt type="disconnect"></mqtt>
        </request>
    </session>

LDAP
^^^^

.. _sec-session-ldap-label:

Authentication
""""""""""""""

The recommended mechanism used to authenticate users against a LDAP
repository requires two steps to follow. Given an username and
password, we:

* Search the user in the repository tree, using the username (so users can reside in different subtrees of the organization)
* Try to bind as the user, with the distinguished name found in the first step and the user's password

If the bind is successful, the user is authenticated (this is the
scheme used, among others, by the LDAP authentication module for
Apache http://httpd.apache.org/docs/2.0/mod/mod_auth_ldap.html)

LDAP Setup
""""""""""

For this example we are going to use a simple repository with the following hierarchy:

.. figure:: ./images/ldap-hierarchy.png

  LDAP hierarchy

The repository has users in two organizational units

#. users (with four members)
#. users2 (with tree members)


For simplicity we set the password of each user to be  the same as its common name (cn).
Tsung Setup
We will use a CSV file as input, containing the user:password pairs
for our test. So we start by writing it, in this case we name the file ``users.csv``::

  user1;user1
  user2;user2
  user3;user3
  user4;user4
  jane;jane
  mary;mary
  paul;pablo
  paul;paul

The pair ``paul:pablo`` should fail to authenticate, we will note that in the Tsung report.
Then, in our Tsung scenario, we let Tsung know about this file:

.. code-block:: xml

   <options>
     <option name="file_server" id="users" value="users.csv"/>
   </options>
   <!-- We use two dynamic variables to hold the username and password -->
   <setdynvars sourcetype="file" fileid="users" delimiter=";" order="iter">
		<var name="username" />
		<var name="password" />
   </setdynvars>


To start the authentication process we instruct Tsung to perform a search, to find the distinguished name of the user we are trying to authenticate

.. code-block:: xml

  <ldap type="search" base="dc=pablo-desktop" filter="(cn=%%_username%%)"
        result_var="search_result" scope="wholeSubtree"></ldap>


As we need to access the search result, we specify it using the ``result_var`` attribute. This attribute tells Tsung in which dynamic variable we want to store the result (if the ``result_var`` attribute isn't set, Tsung doesn't store the search result in any place).
Finally,  we try to bind as that user.

.. code-block:: xml

 <request subst="true">
   <ldap type="bind" user="%%ldap_auth:user_dn%%"
         password="%%_password%%"></ldap>
 </request>


The only thing that remains to do is to implement the ``ldap_auth:user_dn`` function, that extract the distinguished name from the search result.

.. code-block:: erlang

 -module(ldap_auth).
 -export([user_dn/1]).
 user_dn({_Pid,DynVars}) ->
      [SearchResultEntry] = proplists:get_value(search_result,DynVars),
      {_,DN,_} = SearchResultEntry,
      DN.


We aren't covering errors here. supposing that there is always one (and only one) user found, that we extract from the ``search_result`` variable (as defined in the previous search operation).
Each entry in the result set is a SearchResultEntry record. The record definition can be found in ``<TSUNG_DIR>/include/ELDAPv3.hrl``.

As we only need to access the distinguished name of the object, we index into the result tuple directly. But if you need to access other attributes you probably will want to include the appropriate .hrl and use the record syntax instead. One of the eight user:password pairs in our users file was wrong, so we expect 1/8 of the authentication attempts to fail.

Indeed, after running the scenario we can confirm this in the Tsung
report (see figure :ref:`fig-ldap-results-label`). The bind operation maintains two
counters: ``ldap_bind_ok`` and ``ldap_bind_error``,
that counts successful and unsuccessful bind attempts.

.. _fig-ldap-results-label:
.. figure:: ./images/ldap-results.png

  LDAP Results

Other examples
""""""""""""""

.. code-block:: xml

 <session probability="100" name="ldap-example" type="ts_ldap">
   <request>
     <ldap type="bind" user="uid=foo" password="bar"/>
   </request>

   <request>
     <ldap type="search" base="dc=pablo-desktop" filter="(cn=user2)"
     scope="wholeSubtree"></ldap>
   </request>

   <!-- Add. Adds a new entry to the directory* -->
   <request subst="true">
     <ldap type="add" dn="%%_new_user_dn%%" >
       <attr type="objectClass">
         <value>organizationalPerson</value>
         <value>inetOrgPerson</value>
         <value>person</value>
       </attr>
       <attr type="cn"><value>%%_new_user_cn%%</value></attr>
       <attr type="sn"><value>fffs</value></attr>
     </ldap>
   </request>

   <!-- Modify. Modifies an existing entry; type=add|delete|modify-->
   <request subst="false">
     <ldap type="modify" dn="cn=u119843,dc=pablo-desktop" >
       <modification type="replace">
         <attr type="sn"><value>SomeSN</value></attr>
         <attr type="mail"><value>some@mail.com</value></attr>
       </modification>
     </ldap>
   </request>
 </session>

.. index:: change_type

Mixing session type
^^^^^^^^^^^^^^^^^^^

Since version **1.3.2**, a new tag **change_type** can be
used in a session to change it's type.


.. code-block:: xml

 <request> 
   <jabber type="chat" ack="no_ack" size="16"
           destination="offline"/> 
 </request>

 <thinktime value="3"/>

 <change_type new_type="ts_http" host="foo.bar" port="80"
 server_type="tcp" store="true"/>

 <request> <http url="http://foo.bar/"/> </request>
 <request> <http url="/favicon"/> </request>

 <change_type new_type="ts_jabber" host="localhost" port="5222"
 server_type="tcp" restore="true"/>

 <request> <jabber type="chat" ack="no_ack" size="16"
 destination="previous"/> </request>


``store="true"`` can be used to save the current state of the session (socket,
cookies for http, …) and ``restore="true"`` to reuse the previous state when
you switch back to the old protocol.

You can use ``bidi="true"`` to indicate that the new protocol is bidirectional or
``bidi="false"`` for a non-bidirectional protocol (only available in version
**1.5.1** and newer).

A dynamic variable set in the first part of the session will be
available after a **<change_type>**. There is currently one caveat: you have
to use a full URL in the first http request after a **<change_type>** (a
relative URL will fail).


Raw
^^^^^^^^^
.. _sec-session-raw-label:

The **ts_raw** plugin allows you to send traffic to any kind of
TCP/UDP server without any knowledge of the underlying protocol. You can set the data
by attribute ``data``, or just set a data size by attribute
``datasize`` (in this situation, Tsung send ``datasize`` bits of
zeros). ``data`` and ``datasize`` can be a dynamic values.

The only way to control the response from the server is to use the
``ack`` attribute (also used by the **jabber** plugin):

* ``ack="local"`` as soon as a packet is received from the server, the
  request is considered as completed. Hence if you use a local ack with a request
  that does not require a response from the server, it
  will wait forever (or until a timeout is reached).

* ``ack="no_ack"`` as soon as the request is sent, it is considered as completed (do
  not wait for incoming data).

* ``ack="global"`` synchronized users. its main use is for waiting for all
  users to connect before sending messages. To do that, set a request
  with global ack  (the value can be set using the option ``<option
  name="global_number" value ="XXX"/>`` and by setting `maxnumber=N` in
  ``<arrivalphase>``).

.. code-block:: xml

 <session probability="100" name="raw" type="ts_raw">
    <transaction name="open">
      <request> <raw data="HELLO" ack="local"></raw> </request>
    </transaction>

    <thinktime value="4"/>
    <request> <raw datasize="2048" ack="local"></raw> </request>

    <transaction name="bye">
      <request> <raw data="BYEBYE" ack="local"></raw> </request>
    </transaction>
 </session>
