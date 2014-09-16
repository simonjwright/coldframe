--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with ColdFrame.Events_G;
generic
   with package Events is new ColdFrame.Events_G (<>);
   --  Instantiate with ColdFrame.Project.Events.
package ColdFrame.Scripted_Testing_G is

   procedure Register (The_Dispatcher : not null Events.Event_Queue_P);
   --  The_Dispatcher needs to be from an instantiation of Test_G, so
   --  that Start_Dispatcher and Wait_Until_Idle work.

   --  The provided commands are:
   --
   --  start_dispatcher: start the registered Event_Queue.
   --
   --  wait_until_idle: wait until there are no more non-held events
   --  on the registered Event_Queue.

end ColdFrame.Scripted_Testing_G;
