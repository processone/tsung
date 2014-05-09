%%%
%%%  Copyright 2014 Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 23 avril 2014 by Nicolas Niclausse <nicolas@niclux.org>
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

-module(ts_api).
-vc('$Id: ts_web.erl,v 0.0 2014/04/23 12:12:17 nniclaus Exp $ ').
-author('nicolas@niclux.org').

-include("ts_macros.hrl").
-include_lib("kernel/include/file.hrl").

-export([status/3]).

status(SessionID, _Env, _Input) ->
    {ok, Nodes, Ended_Beams, MaxPhases} = ts_config_server:status(),
    Active    = Nodes - Ended_Beams,
    {Clients, ReqRate, Connected, Interval, Phase, Cpu} = ts_mon:status(),
    NPhase = case Phase of
                 error -> 1;
                 {ok,N} -> (N div Nodes) + 1
             end,
    JSON = "{\"phase\": "++ integer_to_list(NPhase) ++ ","
         ++ "\"phase_total\": "++ integer_to_list(MaxPhases) ++ ","
         ++ "\"users\": "++ integer_to_list(Clients) ++ ","
         ++ "\"connected_users\": "++ integer_to_list(Connected) ++ ","
         ++ "\"request_rate\": "++ ts_web:number_to_list(ReqRate/Interval) ++ ","
         ++ "\"active_beams\": "++ integer_to_list(Active) ++ ","
         ++ "\"cpu_controller\": "++ ts_web:number_to_list(Cpu)
         ++ "}",
    mod_esi:deliver(SessionID, [
                                "Content-Type: application/json\r\n\r\n",
                                JSON
                               ]).


