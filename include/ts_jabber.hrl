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
%%%  the two.

-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-record(jabber_dyndata, {id}).

-record(jabber, {dest,
				 size,
                 data,
				 type,
				 jud_param,
				 cle,
				 id = 0,
				 domain, %% jabber domain
				 username, %% first chars of username (will append id dynamically)
				 passwd   %% first chars of passwd (will append id dynamically)
				}).

-define(setroster_intensity, 1/(ts_utils:get_val(setroster)*1000)).
