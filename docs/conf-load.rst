.. index:: load
.. _load-label:

Defining the load progression
-----------------------------


Randomly generated users
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: arrivalphase

The load progression is set-up by defining several arrival phases:

.. code-block:: xml

   <load>
     <arrivalphase phase="1" duration="10" unit="minute">
       <users interarrival="2" unit="second"></users>
     </arrivalphase>
   
     <arrivalphase phase="2" duration="10" unit="minute">
       <users interarrival="1" unit="second"></users>
     </arrivalphase>
   
     <arrivalphase phase="3" duration="10" unit="minute">
       <users interarrival="0.1" unit="second"></users>
     </arrivalphase>
   </load>


With this setup, during the first 10 minutes of the test, a new user
will be created every 2 seconds, then during the next 10 minutes, a
new user will be created every second, and for the last 10 minutes,
10 users will be generated every second. The test will finish when
all users have ended their session.

You can also use :index:`arrivalrate` instead of
:index:`interarrival`. For example, if you want 10 new users per
second, use:

.. code-block:: xml

  <arrivalphase phase="1" duration="10" unit="minute">
    <users arrivalrate="10" unit="second"></users>
  </arrivalphase>

You can limit the number of users started for each phase by using the
:index:`maxnumber` attribute, just like this:

.. code-block:: xml

   <arrivalphase phase="1" duration="10" unit="minute">
     <users maxnumber="100" arrivalrate="10" unit="second"></users>
   </arrivalphase>
   <arrivalphase phase="2" duration="10" unit="minute">
     <users maxnumber="200" arrivalrate="10" unit="second"></users>
   </arrivalphase>

In this case, only 100 users will be created in the first phases, and
200 more during the second phase.


The complete sequence can be executed several times using the
``loop`` attribute in the ``load`` tag
(``loop='2'`` means the sequence will be looped twice, so the
complete load will be executed 3 times) (feature available since
version 1.2.2).

The load generated in terms of HTTP requests / seconds will also
depend on the mean number of requests within a session (if you have a
mean value of 100 requests per session and 10 new users per seconds,
the theoretical average throughput will be 1000 requests/ sec).

.. versionadded:: 1.5.1

You can also override the probability settings of sessions within a
specific phase, using ``session_setup``:

.. code-block:: xml

    <arrivalphase phase="3" duration="1" unit="minute">
      <session_setup name="http_test_1" probability="80"/>
      <session_setup name="fake"        probability="20"/>
      <users  interarrival="1" unit="second"/>
    </arrivalphase>


.. index:: start_time

Statically generated users
^^^^^^^^^^^^^^^^^^^^^^^^^^

If you want to start a given session (see :ref:`sessions-label`) at a given time during the test,
it is possible since version **1.3.1**:

.. code-block:: xml

 <load>
   <arrivalphase phase="1" duration="10" unit="minute">
     <users interarrival="2" unit="second"></users>
   </arrivalphase>
   <user session="http-example" start_time="185" unit="second"></user>
   <user session="http-example" start_time="10" unit="minute"></user>
   <user session="foo" start_time="11" unit="minute"></user>
 </load>
 <sessions>
   <session name="http-example" probability="0" type="ts_http">
     <request> <http url="/" method="GET"></http> </request>
   </session>
   <session name="foobar" probability="0" type="ts_http">
     <request> <http url="/bar" method="GET"></http> </request>
   </session>
   <session name="foo" probability="100" type="ts_http">
     <request> <http url="/" method="GET"></http> </request>
   </session>
 <sessions>


In this example, we have two sessions, one has a "0" probability (and
therefore will not be used in the first phase), and the other
100\%. We define 3 users starting respectively 3mn and 5 seconds
after the beginning of the test (using the ``http-example``
session), one starting after 10 minutes, and a last one starting after
11 minutes (using the ``foo`` session this time)

.. versionadded:: 1.5.1

If you want to start several sessions at once, and if the name of
these sessions starts with the same prefix, you can use a
wildcard. Given the previous sessions, this example will start two
users (one with ``foo`` session, and one with ``foobar`` session) at
starttime +10s.

.. code-block:: xml

    <user session="foo*" start_time="10" unit="second"/>


.. index:: duration

Duration of the load test
^^^^^^^^^^^^^^^^^^^^^^^^^

By default, tsung will end when all started users have finished their
session. So it can be much longer than the duration of
arrivalphases. If you want to stop Tsung  after a given duration
(even if phases are not finished or if some sessions are still actives),
you can do this with the ``duration`` attribute in ``load`` (**feature added in 1.3.2**):

.. code-block:: xml

   <load duration="1" unit="hour">
     <arrivalphase phase="1" duration="10" unit="minute">
       <users interarrival="2" unit="second"></users>
     </arrivalphase>
   </load>


Currently, the maximum value for duration is a little bit less than 50
days. ``unit`` can be ``second``, ``minute`` or ``hour``.
