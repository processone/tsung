%%%
%%%  Copyright © IDEALX S.A.S. 2003
%%%
%%%	 Author : Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
%%%  Created: 03 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
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
%%%  the two.


-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-define(TSUNAMIPATH, "TSUNAMIPATH").
-define(SESSION_POP_ERROR_MSG, "Total sum of session popularity is not equal to 100").

-record(config,	{
          name,
          loglevel = ?WARN,
          dump = none,
          stats_backend,
          controller, %% controller machine
          clients=[], %% client machines

          server, %% server to test
          monitor_hosts = [], %% Cluster host to monitor (for CPU, MEM usage)
          arrivalphases = [], %% arrival process specs
          thinktime,  %% default thinktime specs
          subst = false, %% Substitution should be applied on the request
          match, %% Match regexp in response
          dynvar,
          sessions=[],
          session_tab,
          curthink, %% temporary var (current request think)
          curid = 0, %% temporary var (current request id (can be transaction))
          cur_req_id = 0, %% temporary var (current real request id)
          file_server, % filename for file_server
          ssl_ciphers = negociate
		}).

-record(client,	
        {host,
         weight   = 1,
         maxusers,
         ip         = []
        }).
-record(server,	
        {host,
         port,
         type
        }).
-record(session,
        { id,
          popularity,
          type,
          messages_ack = parse,
          persistent   = false,
          ssl_ciphers  = negociate,
          size 
        }).

-record(arrivalphase,
        {phase,
         duration,
         unit,
         number, %% ?
         intensity,
         maxnumber
        }).
