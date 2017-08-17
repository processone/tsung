# Changelog

## [Unreleased]
### Fixed
### Changed
### Added
- [#189] Add direct-ip support for tsung nodes interconnection
- #201 add option to start a phase after all generated users in the previous phase have finished their session

## [1.6.0] - 2015-07-20 - Major enhancements and bugfixes
### Fixed
- [TSUN-225] - SSL Session Caching Issues
- [TSUN-292] - Indecipherable error with no arrivalphase elements
- [TSUN-294] - Logging(?) of unmatched dyn vars puts lot of pressure on controller
- [TSUN-295] - tsung status crashes test run
- [TSUN-296] - Reported response size in dumpfile seems to be way too low
- [TSUN-297] - Float values in thinktimes substitution
- [TSUN-305] - Can't connect with +TLS to ejabberd/XMPP
- [TSUN-308] - Handle ssl_closed in ts_client
- [TSUN-309] - RNG Seeding is too weak and causes collisions
- [TSUN-312] - Handle cast new beam failed
- [TSUN-316] - LDAP scenarios fail when compiled with R16A/B due to asn1rt_ber_bin
- [TSUN-320] - get_os_data(freemem, {unix, linux}) crashes on Linux 3.10.0
- [PR #91]   - [MQTT] Last Will and Testament should not be included if will_topic is not set.
- [PR #104]  - Fix problem with MQTT SUBACK packages
- [PR #107]  - Fix crash when use_controller_vm="false" and controller id is not empty
- [PR #109]  - Fix race condition in ts_utils:make_dir/1

### Changed
- [TSUN-307] - Allow all HTTP headers to be overridden by <http_header>
- [PR #111]  - Add subst for amqp exchange, routingKey and queue name
- [PR #79]   - added support for mqtt user and password
- [PR #81]   - Allow setting of dynamic vars for MQTT's username and password
- [PR #93]   - Add two config option

### Added
- [TSUN-290] - Add a web dashboard embedded in tsung controller
- [TSUN-293] - Enable node local dumptraffic log
- [TSUN-304] - Add command line option to add additional erlang module load paths
- [TSUN-306] - Add connection_timeout option
- [PR #106]  - XMPP message latency measurement

## [1.5.1] - 2014-04-07 - Major enhancements and bugfixes
### Fixed
- [TSUN-250] - BOSH Crash
- [TSUN-252] - Too many requests when using max_restart
- [TSUN-253] - Code blocks in html version of user manual is unreadable
- [TSUN-256] - Unexpected ack="global" behaviour on BOSH
- [TSUN-265] - Wrong header line in tsung.dump
- [TSUN-270] - Substitution not working in <websocket/> path attribute
- [TSUN-271] - SSL does not work with erlang >= R16
- [TSUN-272] - Support literal IPv6 addresses when defining servers
- [TSUN-278] - Tsung 1.5.0 is notable to do https out of the box
                 when it is compiled from tarballs
- [TSUN-279] - Tsung 1.5.0 is not able to do substitution of hostname
                  or port in a URL. It only can do substitution of path
- [TSUN-281] - Fix debian build for Tsung 1.5, replaces DocBook with Sphinx
- [TSUN-285] - In some rare conditions in a distributed setup,
                 Tsung fails to start the load test.
- [TSUN-287] - request.max statistic lower than request.mean
- [PR #71]   - oAuth bug fix, PUT method
- [PR #41]   - Fix websocket path subst
- [PR #44]   - Add bidi attribute to change_type
- [PR #49]   - Fix websocket close issue: we should wait a close response

### Changed
- [TSUN-255] - Fix unused vars in tq_amqp
- [TSUN-259] - Tsung in Fedora
- [TSUN-268] - Use xmerl_sax_parser:file/2 in case of xml parsing error
- [TSUN-276] - Add text frame support for websocket
- [TSUN-284] - Do not use boot files to start tsung and
                 tsung_controller applications
- [PR #51]   - Updated dygraph charting library to the latest release
- [PR #65]   - AMQP: add multiple channel, add waitForConfirms and
                 waitForMessages
- [PR #70]   - Add bosh_path config option
- [PR #74]   - Add text frame support for Websocket, and update doc

### Added
- [TSUN-260] - Add option to change popularities of sessions for each phase
- [TSUN-264] - New comparison operators
- [TSUN-269] - Logging of request tags to dumpfile
- [TSUN-275] - Add MQTT support
- [TSUN-280] - Tsung to support pkcs#12 certificates or at least cacerts, clientcerts and keys
- [PR #42]   - Adding all_except_body' option to ts_http request subst.
                 Adding mysqladmin monitoring options to erlang monitors.
                 Adding mean rate calculation to tsung_stats reports.
                 Adding --title option to set header of report
- [PR #75]   - Support SSL/TLS client certificate file attributes for jabber starttls

## [1.5.0] - 2013-05-24 - Major enhancements and bugfixes
### Fixed
- [TSUN-208] - in the jabber plugin, substitutions for raw request doesn't work in some cases.
- [TSUN-209] - If tag doesn't work with Tsung 1.4.2
- [TSUN-212] - Incorrect ERTS version being set on build.
- [TSUN-215] - normal ack timeout shouldn't used for global ack
- [TSUN-217] - If statement breaks on empty string
- [TSUN-218] - Race condition in tsung-recorder
- [TSUN-219] - Site fails to load via proxy recorder
- [TSUN-220] - Large configuration files trigger error
- [TSUN-229] - compatibility with erlang R15B
- [TSUN-230] - can't connect with TLS + ejabberd
- [TSUN-232] - Tsung for bosh protocol doesn't send a empty request to keep the user session alive.
- [TSUN-234] - Error encoding json string with escape_uri
- [TSUN-238] - Content-Length parsing broken
- [TSUN-241] - Invalid link Other in the graph.html
- [TSUN-245] - Message when dtd is not found not trivial

### Changed
- [TSUN-174] - add an option to set resource in XMPP
- [TSUN-222] - Support unsubscribe operation for Jabber pubsub module
- [TSUN-228] - allow substitutions on cookies
- [TSUN-236] - Add probability support for servers
- [TSUN-242] - add timestamp and request duration in dump=protocol for http
- [TSUN-246] - http PATCH support

### Added
- [TSUN-214] - Ability to pass attributes for node creation for XMPP pubsub protocol.
- [TSUN-227] - add new dynamic variable to get server hostname and port
- [TSUN-231] - add option to use weights instead of probabilities for sessions
- [TSUN-239] - add BOSH support
- [TSUN-240] - add websocket support
- [TSUN-244] - Percentile computation
- [TSUN-248] - add AMQP support

## [1.4.2] - 2012-01-04 - Minor enhancements and bugfixes
### Fixed
 - [TSUN-199] - computation of NUsers is wrong
 - [TSUN-206] - build failure with erlang R15B

### Changed
 - [TSUN-202] - IPv6 support
 - [TSUN-203] - snmp oids should be customizable in the config file
 - [TSUN-205] - handle dyn_variables as array in test conditions (if/until/while)

### Added
 - [TSUN-191] - allow outputting log to stdout
 - [TSUN-192] - structured log output (JSON)
 - [TSUN-193] - accept configuration from stdin
 - [TSUN-197] - Have bug and error message on stderr and not stdout.

## [1.4.1] - 2011-09-13 - Minor bugfixes
### Fixed
 - [TSUN-188] - munin plugin is not working in 1.4.0
 - [TSUN-189] - the controller VM is not used in some case
 - [TSUN-190] - pgsql recorder can record a connect request in an already connected session

## [1.4.0] - 2011-09-05 - Major enhancements and bugfixes
### Fixed
 - [TSUN-129] - regexp (defined in match or dynvars) can fail when chunk encoding is used.
 - [TSUN-150] - Munin monitoring broken by cpu stats config request
 - [TSUN-163] - Tsung doesn't detect subdomains.
 - [TSUN-166] - snmp monitoring does not work with erlang R14A
 - [TSUN-171] - maxnumber set in a phase is not always enforced
 - [TSUN-172] - auth sasl can't authenticate against ejabberd
 - [TSUN-178] - some characters can make url and headers rewriting fail in the recorder
 - [TSUN-179] - tsung generated message stanzas are not XMPP compliant
 - [TSUN-180] - file server crash if a dynamic substitution use it
 - [TSUN-182] - When many clients are configured with few static users, none of them are launched.
 - [TSUN-183] - tsung can stop too soon in some cases
 - [TSUN-184] - 'random_number' with start and end actually returns a number from start+1 to end
 - [TSUN-187] - Client IP scan is very slow on Linux; also uses obsolete "ifconfig"

### Changed
 - [TSUN-54] - tsung is very slow when a lot of dynamic variables are set
 - [TSUN-96] - generating more than 64k connections from a single machine
 - [TSUN-106] - Add content-encoding support for dynvar extraction
 - [TSUN-123] - add option to read usernames from an external file for jabber
 - [TSUN-125] - use the new re module everywhere instead of regexp/gregexp
 - [TSUN-152] - Add "state_rcv" record as parameter to "get_message" function.
 - [TSUN-153] - dynvar used in match may contain regexp special characters
 - [TSUN-185] - handle postgresql extended protocol

### Added
 - [TSUN-162] - add foreach tag (loop when a dyn_variable is a list)
 - [TSUN-164] - add a switch to allow light queries/replies logging
 - [TSUN-165] - add a way to synchronize users for all plugins.
 - [TSUN-167] - add do=dump option to matching
 - [TSUN-168] - thinktimes value could be dynamically generated with dyn_variable
 - [TSUN-181] - add option to simulate slow connections

## [1.3.3] -  2010-08-17 - Minor bugfixes
### Fixed
- [TSUN-154] - parent proxy doesn't work anymore in 1.3.x (tested with 1.3.2 and 1.3.0).
- [TSUN-155] - url substitution is broken in some cases
- [TSUN-156] - Tsung not using sessions with low probabilities
- [TSUN-157] - ssl doesn't work with erlang R14A
- [TSUN-158] - failure when a proxy is used and an URL substitution is set
- [TSUN-159] - HTTP cookies support is broken when a proxy is used
- [TSUN-160] - tsung can sometimes hang at the beginning using distributed setup
- [TSUN-161] - if statement not allowed in a transaction

## [1.3.2] - 2010-06-14 - Major bugfixes and enhancements
### Fixed
- [TSUN-128] - Apostrophes cause string to convert to deep list in setdynvars with Erlang function.
- [TSUN-130] - static users starting time is wrong
- [TSUN-131] - tsung can stop too early when static users are used
- [TSUN-132] - http cookies: accept domains equals to hostname with a leading "."
- [TSUN-133] - proxy-recorder with SSL fails on large client packets (multiple TCP packets)
- [TSUN-138] - when an error occured( for ex a timeout during a request) and a client exits, started transactions are not updated
- [TSUN-140] - tsung does not honor the Proxy-Connection: keep-Alive or Connection: keep-Alive header if the proxy is HTTP/1.0
- [TSUN-142] - http recorder can fail with https rewriting and chunked encoding
- [TSUN-147] - UDP & bidi does not seem to work
- [TSUN-148] - dynvar not found when used in match
- [TSUN-149] - tsung doesn't work with Amazon Elastic load balancing
- [TSUN-151] - tsung_stats.pl produces invalid *.gplot files

### Changed
- [TSUN-82]  - XMPP vhost support
- [TSUN-127] - add ability tu use floats for thinktimes
- [TSUN-139] - option to set random seed for launchers.
- [TSUN-141] - add dynamic variable support for postgres
- [TSUN-146] - New tsplot yfactor configuration support breaks most common configurations

### Added
- [TSUN-135] - add support for SASL ANONYMOUS and PLAIN authentication for XMPP
- [TSUN-136] - add new plugin distributed testing of filesystem (nfs for ex), using a generic mode for executing erlang functions on clients nodes
- [TSUN-137] - add a way to mix requests types inside a single session
- [TSUN-143] - global time limit for the load test
- [TSUN-145] - tsung should support json parsing for dynamic variable

## [1.3.1] - 2009-09-09 - Major bugfixes and enhancements
### Fixed
- [TSUN-92] - the computation of the minimum for sample_counter is wrong
- [TSUN-93] - maxnumber not respected if several clients are used
- [TSUN-102] - dyn_variable configuration fails if variable name is not a valid erlang atom
- [TSUN-103] - Network error handling in munin plugin
- [TSUN-104] - tsung-plotter can't handle the os_mon statistics
- [TSUN-108] - Cannot handle complicated dyn_var name
- [TSUN-109] - Tsung status displays always phase one even if you have more than one phase
- [TSUN-110] - Cookie header not present if the URL is dynamically generated by a previous redirection (302)
- [TSUN-117] - Bug in HTTP: empty header can be generated in some case
- [TSUN-118] - HTTPS proxy recorder: ts_utils:to_https incorrectly handles Content-Length for POST requests
- [TSUN-119] - tsung can crash when reading empty values from a csv file
- [TSUN-122] - same http cookie key with different domains don't work

### Changed
- [TSUN-47] - ts_mon can be a bottleneck during very high load testing
- [TSUN-77] - Structural requests or goto-like action for match in HTTP
- [TSUN-81] - Dynamic variables API
- [TSUN-83] - file_server using fixed tuple instead of list
- [TSUN-85] - external entity should be copied into the log directory of a run.
- [TSUN-87] - add dynamic code evaluation in set_dynvars
- [TSUN-88] - add mkactivity method support in webdav
- [TSUN-91] - reduce memory consumption by hibernating client process while in think state
- [TSUN-97] - disable smp erlang for client beam for performance reason
- [TSUN-98] - try several times to connect to the server before aborting a session
- [TSUN-99] - make substitution work in <match>
- [TSUN-100] - improve scalability of ts_launcher
- [TSUN-105] - Add load average statistic to server monitoring
- [TSUN-111] - add option to manually add a cookie in http requests
- [TSUN-113] - split tsung command into two separate tsung and tsung-recorder commands
- [TSUN-116] - add ability to run several tsung running in parallel on the same hosts
- [TSUN-120] - Https recorder: Remove "Secure" from "Set-Cookie" header.

### Added
- [TSUN-25] - add a way to start sessions in a specific order at specified times
- [TSUN-89] - include tsung-plotter into the tsung distribution
- [TSUN-90] - add support for monitoring server cpu/mem using munin-node
- [TSUN-94] - add log action for match
- [TSUN-95] - add a default dyn_variable with a unique tsung_userid
- [TSUN-107] - add MUC support to the jabber doc/plugin
- [TSUN-114] - add option to apply function to data before looking for a match
- [TSUN-115] - add pubsub support to the jabber plugin

## [1.3.0] - 2008-09-03 - Major bugfixes and enhancements
### Fixed
- [TSUN-30] - SNMP monitoring gives an error
- [TSUN-57] - using -l with a relative path make distributed load fails with timeout error
- [TSUN-60] - https recorder broken if an HTML document includes absolute urls
- [TSUN-67] - Typo breaks recording of if-modified-since headers
- [TSUN-68] - some sites doesn't work with ":443" added in the "Host" header with https
- [TSUN-71] - Tsung does not work with R12B (httpd_util funs removed)
- [TSUN-73] - Wrong parsing HTTP multipart/form-data in http request - POST form doesn't work
- [TSUN-75] - can not define more -pa arguments
- [TSUN-84] - dyn variables that don't match should be set to an empty string

### Changed
- [TSUN-40] - problem to rewrite url for https with gzip-encoded html.
- [TSUN-48] - tcp/udp buffer size should be customizable in the XML config file.
- [TSUN-59] - if a User-Agent header is set in <header>, it should override the global one.
- [TSUN-62] - add abilty to loop back to a previous request in a session
- [TSUN-63] - check for ssl and crypto application at compile time
- [TSUN-65] - enhance dynamic variables.
- [TSUN-66] - add global mean and counter computation and reporting for samples
- [TSUN-69] - add option to read content of a POST request from an external file
- [TSUN-79] - setting 'Host' header with http_header doesn't work as expected

### Added
- [TSUN-56] - ldap plugin
- [TSUN-58] - add a new statistics backend to dump all stats in a file
- [TSUN-61] - add a Webdav plugin
- [TSUN-64] - add md5 authentication in the pgsql plugin
- [TSUN-72] - Add support for defining dyn_variables using XPath
- [TSUN-78] - mysql plugin
- [TSUN-80] - add random thinktime with in a given range ( [min,max])
- [TSUN-76] - add explanation for errors name in the documentation

### [1.2.2] - 2008-02-23 - Minor bugfixes and enhancements
### Fixed
- [TSUN-30] - SNMP monitoring gives an error
- [TSUN-31] - dyn_variable usage
- [TSUN-35] - udp is not working
- [TSUN-36] - default regexp for dyn_variable doesn't work in all case
- [TSUN-38] - server monitoring crash if an ethernet interface's name is more than 6 chars long
- [TSUN-39] - https recording doesn't work with most browsers
- [TSUN-43] - session should not terminate if rosterjid is not defined
- [TSUN-49] - <match> doesn't work with jabber plugin
- [TSUN-51] - tsung does not work with R12B (httpd_util funs removed)
- [TSUN-53] - postgresql errors not reported in all cases
- [TSUN-55] - no error counter when userid_max is reached

### Changed
- [TSUN-14] - no_ack messages and asynchronous msg sent by the server are not available in the reports
- [TSUN-27] - handle bidirectional protocols
- [TSUN-28] - Refactoring needed to ease the change of the userid / password generation code
- [TSUN-29] - Multiple file_server support
- [TSUN-32] - make snmp server options tunable
- [TSUN-34] - add costum http headers
- [TSUN-44] - tsung should ignore whitespace keepalive from xmpp server
- [TSUN-45] - add kernel-poll support for better performance
- [TSUN-46] - add number of open connections in statistics
- [TSUN-47] - ts_mon can be a bottleneck during very high load testing
- [TSUN-50] - use the whole range of Id (from 0 to userid_max) before reusing already used Ids

### Added
- [TSUN-26] - Ability to loop on a given sequence of phase
- [TSUN-52] - Adding comment during script capture
- [TSUN-41] - add support for parent proxy for http only (not https)

## [1.2.1] - 2006-10-07 - Minor bugfixes and enhancements
### Fixed
- [TSUN-5]  get traffic from all interfaces instead of only eth0
      in erlang os monitoring (Linux)
- [TSUN-18 the pgsql recorder fails if the client doesn't try
      first an SSL connection
- [TSUN-19] a % character in some requests (eg. type=sql for
      pgsql) make the config_server crash.
- [TSUN-20] pgsql client fails while parsing data from server
- [TSUN-21] substitution in URL is not working properly when a new server or port is set
- [TSUN-23] set default http version (1.1)
- [TSUN-24] destination=previous doesn't work (jabber)

### Changed
- [TSUN-15] listen port is now customizable with the command line
- [TSUN-17] add option to setup postgresql server IP and port at runtime for the recorder
- [TSUN-22] add support for PUT, DELETE and HEAD methods for http

## [1.2.0] - 2006-05-29 - Major feature enhancements
### Fixed
- fix beams communication problem introduced in new erlang releases.
- fix several small problems with 'use_controller_vm' option
- fix regression in recorder for WWW-Authentication (anders.nygren@gmail.com)
- fix presence:roster request
- fix online: must use presence:initial to switch to online status
- fix single user agent case.
- minor fixes for HTTP parsing

### Changed
- change name: idx-tsunami is now called tsung
- import snmp_mgr src from R9C2 to enable SNMP with R10B
- rebuild boot scripts if erlang version is different from compile time
- many DTD improvements
- improved match: add loop|abort|restart on (no)match behavior,
      multiple match tags is now possible (suggested by msmith@truelink.com)
- ip is no more mandatory (default is 0.0.0.0)
- close client socket when connection:closed is ask by the server
      (this should enable https recording with IE)
- roster enhancements (jasonwtucker@gmail.com)

### Added
- add new plugin: pgsql for postgresql load testing
- new: it's now possible to set multiple servers (selected at runtime
      by round robin)
- add size_rcv stats
- freemem and packet stats for Solaris (jasonwtucker@gmail.com)
- clients and monitoring can use hosts list defined in environment
      variables, for use with batch schedulers (openpbs/torque, LSF and OAR)
- performance improvements in stats engine for very high load
      (use session_cache)
- add plugin architecture in recorder; add pgsql plugin
- add presence:directed , presence:broadcast & presence:final requests
      for jabber (jasonwtucker@gmail.com)
- sip-digest authentication (jasonwtucker@gmail.com)
- add pubsub support (mickael.remond@process-one.net)

## [1.1.0] - 2005-09-05 - Major feature enhancements

### Added
- new feature: HTTP proxy load testing in now possible (set
      http_use_server_as_proxy to true)
- add dynamic substitution support for jabber
- add 'raw' type of msg for Jabber (use the new 'data' attribute)
- UserAgent is now customizable for HTTP testing

### Changed
- add the dynamic variable list to dynamic substitutions
- Add an option to run all components (controller and launcher)
      within a single erlang beam (use_controller_vm). Should ease
      idx-tsunami use for light load tests
- internal: Host header is now set during configuration phase

### Fixed
- fix bash script for solaris (jasonwtucker@gmail.com)
- fix: several 'idx-tsunami status' can be run simultaneously
      (reported by Adam Spotton)
- fix last phase duration
- fix recorder: must log absolute url if only the scheme has changed

## [1.0.3] - 2005-07-08 - Minor bugfixes

### Fixed
- fix broken https recording
    Thx to johann.messner@jku.at for bug reporting :
- fix: forgot to add "?" when an URL is absolute and had a query
      part
- fix regression in the recorder (introduced in 1.0.2): must use CAPS
      for method, wrong content-length in recorder causing POST requests
      to silently fail
- fix Host: header when port is != 80

### Added
- add ts_file_server module

### Changed
- allow multiple 'dyn_variable' in DTD

## [1.0.2] - 2005-06-06 - Minor bugfixes

### Fixed
- fix: the recorder is working now with R10B: replace call to
      httpd_parse:request_header in recorder by an
      internal func (the func was removed in R10B)
- update configure scripts (should build on RHEL3/x86_64)

### Changed
- remote beam startup is now tunable (-r ssh/rsh)
- internal changes in ts_os_mon (suggested by R. Lenglet)

## [1.0.1] - 2004-11-18 - Major bugfixes

### Fixed
- fix: broken free mem on non linux arch (Matthew Schulkind)
- small fixes to the DTD
    Thx to Jonathan Bresler for testing and bug reporting :
- fix: broken 'global', 'local' and 'no_ack' requests and size computation
- fix: broken ids in jabber messages
- fix: broken online/offline in user_server
- default thinktime can now be overriden

### Added
- add script to convert apache log file (combined) to idx-tsunami XML

### Changed
- improved configure: add --with-erlang option and xmerl PATH detection
       idx-tsunami now compiles both with R9C and R10B
- many improvements/fixes in analyse_msg.pl

## [1.0] - 2004-08-13 -  Minor bugfixes

### Fixed
- fix: broken path when building debian package
- fix add_dynparams for jabber

### Added
- add rpm target in makefile
- implement status
- add 'match' in graph and doc

## [1.0.beta7] - 2004-07-20 - Minor bugfixes

### Fixed
- HTTP: really (?) fix parsing of no content-length with connection:close
- better handling of configure (--prefix is working)
- fix: ssl_ciphers option is working again

### Changed
- add different types of output backend (currently, only 'text'
      works; 'rrdtool' is started but unfinished)

## [1.0.beta6] - 2004-05-05 - Minor feature enhancements

### Added
- add a DTD for the configuration file
- add dynamic request substitution (mickael.remond@erlang-fr)
- add dynamic variable parsing from response (can be used
      later in the session for request substitution)
- add response pattern to match (log if not match)

### Fixed
- HTTP: fix partial header parsing (mickael.remond@erlang-fr.org)
- HTTP: fix chunk parsing when the chunk-size is split across two packets
- HTTP: fix parsing of no content-length with connection:close case
- fix: do not connect in init anymore; this fix too long phases when
      connection time is high.

### Changed
- check for bad input (config file, <client> name)
- merge client and client_rcv processes into a single process
- connect stat is now for both new connections and reconnections
- check phase duration in launcher
- various code cleanup

## [1.0.beta5] - 2004-03-25 - Major Feature enhancements
### Added
- add SNMP monitoring (not yet customizable)
- SOAP Support: IDX-Tsunami can now record and replay SOAP HTTP
      scenario. The SOAPAction HTTP header is now recorded

### Fixed
- fix remote start: log filename is now encoded to avoid bad
      parsing of log_file by 'erl'
    Patches from mickael.remond@erlang-fr.org :
- Added ~/.idx-tsunami creation in idx-tsunami script if the directory
      does not already exist
- HTTP: fix Cookie support: Cookie are not necessarily separated by "; "
- HTTP: fix long POST request in the recorder: dorecord message
      was missing enclosing curly brackets, and the body length counter
      were mistakenly taking the header size in its total

### Changed
- Extension of XML attribute entity normalisation
- HTTP: Content-type support in the recorder (needed to handle
     non-HTML form encoded posts)
- add autoconf support to detect Erlang installation path
- Preliminary Windows support: A workaround has been introduced in
      the code to handle behaviour difference between Erlang Un*x and
      Erlang Windows on how the command-line is handled. When an
      assumtion is made on the string type of a parameter, it should be
      check that this is actually a string and not an atom.

## [1.0.beta4] - 2004-03-16 - Minor bugfixes
### Fixed
- fix lost cookie when transfer-encoding:chunked is used
- fix config parsing (the last request of the last page of a
      sesssion was not marked as endpage)
- don't crash anymore on error during start or stop

## [1.0.beta3] - 2004-02-24 - Minor feature enhancements

### Fixed
- fix stupid bug in start script for recorder
- HTTP: fix '&' writes in the XML recorder for 'content' attribute

### Changed
- HTTP: enhanced Cookies parsing ('domain' and 'path' implemented).
- ssl_ciphers can be customized
- change log directory structure: all log files in one directory per test
- change stats names:  page_resptime -> page, response_time -> request

### Added
- add HTML reports (requires the perl Template toolkit)

## [1.0.beta2] - 2004-02-11 - Minor feature enhancements

### Changed
- reorganise the sources
- add tools to build a debian package
- fix documentations
- add minimalistic man page
- syntax change: GETIMS +date replace by GET +'if_modified_since'

## [1.0.beta1] - 2005-02-03 -  Major Feature Enhancements

### Added
- rewrite the configuration engine. Now use an XML file.
- add recording application: use as a HTTP proxy to record session into XML format
- add support to OS monitoring (cpu, memory, network). Currently, use an
      erlang agent on the remote nodes; SNMP is on the TODO list.
      (mickael.remond@erlang-fr.org)
- can now use several IPs per client host
- several arrival phases can be set with different arrival rates and duration
- can set test duration instead of number of users
- add user defined statistics using a 'transaction' tag

### Fixed
- HTTP: fix cookies and POST handling (mickael.remond@erlang-fr.org)
- HTTP: rewrite the parser (faster and cleaner)
- fix bad timeout computation when close occur for persistent client
- bugfixes and other enhancements.
- fix memory leak with ssl (half-closed connections)

## [0.2.1] - 2003-12-09 - Minor bugfixes and small enhancements

### Changed
- optimize session memory consumption: use an ets table to store session setup
- HTTP: preliminary chunked-encoding support in HTTP/1.1
- HTTP: Absolute URL are handled (server and port can be overridden )
- no more .hosts.erlang required
- add stats on simultaneous users

### Fixed
- HTTP: fix crash when content-length is not set in headers
- HTTP: fix POST method

## [0.2.0] - 2003-08-29 - Major Feature Enhancements

### Added
- add 'realtime' stats
- add new 'parse' type of protocol
- add reconnection support (persistent client)
- add basic HTTP and HTTPS support
- split the application in two parts: a single controller (tsunami_controller),
      and the clients (tsunami)
- switch to R9C


## [0.1.1] - 2002-08-13 -  Bugfix realease

### Fixed
- fix config file
- fix few typos in docs
- fix init script
- few optimizations in user_server.erl
- switch to R8B

## [0.1.0] - 2001-05-30 - Initial release

[Unreleased]: https://github.com/processone/tsung/compare/v1.6.0...HEAD
[1.6.0]: https://github.com/processone/tsung/compare/v1.5.1...v1.6.0
[1.5.1]: https://github.com/processone/tsung/compare/v1.5.0...v1.5.1
[1.5.0]: https://github.com/processone/tsung/compare/v1.4.2...v1.5.0
[1.4.2]: https://github.com/processone/tsung/compare/v1.4.1...v1.4.2
[1.4.1]: https://github.com/processone/tsung/compare/v1.4.0...v1.4.1
[1.4.0]: https://github.com/processone/tsung/compare/v1.3.3...v1.4.0
[1.3.3]: https://github.com/processone/tsung/compare/v1.3.2...v1.3.3
[1.3.2]: https://github.com/processone/tsung/compare/v1.3.1...v1.3.2
[1.3.1]: https://github.com/processone/tsung/compare/v1.3.0...v1.3.1
[1.3.0]: https://github.com/processone/tsung/compare/v1.2.2...v1.3.0
[1.2.2]: https://github.com/processone/tsung/compare/v1.2.1...v1.2.2
[1.2.1]: https://github.com/processone/tsung/compare/v1.2.0...v1.2.1
[1.2.0]: https://github.com/processone/tsung/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/processone/tsung/compare/v1.0.2...v1.1.0
[1.0.2]: https://github.com/processone/tsung/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/processone/tsung/compare/v0.2.1...v1.0.1
[0.2.1]: https://github.com/processone/tsung/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/processone/tsung/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/processone/tsung/compare/v0.1.0...v0.1.1
