%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-record(match,
        { regexp,
          subst = false,
         'when' = false,
		  name,
          do    = continue, %(continue | loop | abort | log )
          sleep_loop,       % in seconds
          apply_to_content,
          skip_headers = no,
          max_loop,
          loop_back,
          max_restart
       }).

-record(ts_request,
        {
         ack,
         subst=false,
         match=[],
         dynvar_specs=[], % [] | [{VarName, Regexp} |...]
         param,
         endpage=false,
         host,       % override global server hostname
         port,       % override global server port
         scheme      % override global server type (ssl or gen_tcp)
        }).

% protocol options
-record(proto_opts,
        {ssl_ciphers   = negociate, % for ssl only
         bosh_path = "/http-bind/",  % for bash only
         websocket_path = "/chat",  % for websocket only
         websocket_frame = "binary",  % for websocket only
         retry_timeout = 10,        % retry sending in milliseconds
         max_retries = 3,           % maximum number of retries
         idle_timeout  = 600000,    % timeout for local ack
         connect_timeout  = infinity,   % timeout for gen_tcp:connect/4 (infinity OR time in milliseconds)
         global_ack_timeout = infinity, % timeout for global ack
         tcp_rcv_size  = 32768,     % tcp buffers size
         tcp_snd_size  = 32768,
         udp_rcv_size,              % udp buffers size
         udp_snd_size,
         certificate = [],          % for ssl
         reuse_sessions = true,     % for ssl
         is_first_connect = true   % whether it's the first connection
        }).

-record(token_bucket,
        {rate,
         burst,
         last_packet_date = 0,
         current_size     = 0
         }).
-define(size_mon_thresh, 524288).   % 512KB
-define(short_timeout, 1).

% state of ts_client gen_server
-record(state_rcv,
        {socket=none, %
         ip,          % local ip to bind to
         timeout,     % ?
         retries=0,   % number of connect retries
         hibernate = 10000, % hibernate if thinktime is >= to this (10sec by default)
         host,        % hostname (or IP) of remote server
         port,        % server port
         protocol,    % gen_udp, gen_tcp or ssl
         proto_opts = #proto_opts{},  %
         bidi = false,% true if bidirectional protocol

         session_id,
         request,     % current request specs
         persistent,  % if true, don't exit when connexion is closed
         timestamp,   % previous message date
         starttime,   % date of the beginning of the session
         count,       % number of requests waiting to be sent
         maxcount,        % number of requests waiting to be sent
         ack_done=false,  % 'true' if the ack was sent, else 'false' (unused if ack=no_ack)
         send_timestamp,  % date when the 'request' was sent
         page_timestamp=0,% date when the first 'request' of a page was sent
         acc=[],     % Accumulator to store temporary unparsable data
                     % (Waiting for more data)
         buffer = <<>>, % buffer when we have to keep the response (we need
                     % all the response to do pattern matching)
         session,    % record of session status; depends on 'clienttype' (cas be used to store data dynamically generated during the
                     % session (Cookies for examples))
         datasize=0,
         id,         % user id
         size_mon_thresh=?size_mon_thresh, % if rcv data is > to this, update stats
         size_mon=0, % current size (used for threshold computation)
         dynvars=[], %
         clienttype, % module name (ts_jabber, etc.)
         transactions=[], % current transactions
         rate_limit, % rate limiting parameters
         dump        % type of dump (full, light, none)
        }).


-record(launcher,
        {nusers,
         phases =[],
         myhostname,
         intensity,
         static_done = false,
         started_users = 0,
         phase_nusers,   % total number of users to start in the current phase
         phase_duration, % expected phase duration
         phase_start,    % timestamp
         phase_id      = 1,
         start_date,
         short_timeout = ?short_timeout,
         maxusers %% if maxusers are currently active, launch a
                  %% new beam to handle the new users
        }).



