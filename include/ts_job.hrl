%%%
%%%  Copyright 2011 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 4 mai 2011 by Nicolas Niclausse <nniclaus@sophia.inria.fr>
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


-author('nicolas.niclausse@inria.fr').

%% use by the client to create the request

-record(job_dyndata,
        {
          fixme
         }
       ).

-record(job, {
          script,
          resources,
          walltime,
          queue,
          duration,
          jobid,
          req, % submit|stat|delete
          type, % oar|torque
          notify_port,
          notify_script,
          name,
          user,
          options,
          args
              }).

-record(job_session, {
          jobid,
          owner,
          submission_time,
          queue_time,
          start_time,
          end_time,
          dump,
          status
         }).


