--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Callback_Manager_Test_Support;
with ColdFrame.Project.Events.Standard.Test_Trace;

procedure Callback_Manager_Test
is

   package CMTS renames Callback_Manager_Test_Support;
   package CMTSIC renames CMTS.Integer_Callbacks;
   package CMTSMIC renames CMTS.Managed_Integer_Callbacks;

   Dispatcher : ColdFrame.Project.Events.Event_Queue_P
     := new ColdFrame.Project.Events.Standard.Test_Trace.Event_Queue;

begin

   ColdFrame.Project.Events.Start (Dispatcher);

   Put_Line ("manager not registered: call_callbacks (1)");
   CMTS.Integer_Callbacks.Call_Callbacks (1);
   ColdFrame.Project.Events.Wait_Until_Idle (Dispatcher,
                                             Ignoring_Timers => True);
   New_Line;

   CMTSMIC.Register (Dispatcher);
   Put_Line ("no callbacks registered: call_callbacks (2)");
   CMTSIC.Call_Callbacks (2);
   ColdFrame.Project.Events.Wait_Until_Idle (Dispatcher,
                                             Ignoring_Timers => True);
   New_Line;

   CMTSMIC.Register (CMTS.Callback_Handler'Access);
   Put_Line ("callback registered: call_callbacks (3)");
   CMTSIC.Call_Callbacks (3);
   ColdFrame.Project.Events.Wait_Until_Idle (Dispatcher,
                                             Ignoring_Timers => True);
   New_Line;

   CMTSMIC.Deregister (CMTS.Callback_Handler'Access);
   Put_Line ("callback deregistered: call_callbacks (98)");
   CMTSIC.Call_Callbacks (99);
   ColdFrame.Project.Events.Wait_Until_Idle (Dispatcher,
                                             Ignoring_Timers => True);
   New_Line;

   CMTSMIC.Clear;
   Put_Line ("callback cleared: call_callbacks (99)");
   CMTSIC.Call_Callbacks (99);
   ColdFrame.Project.Events.Wait_Until_Idle (Dispatcher,
                                             Ignoring_Timers => True);
   New_Line;

   --  Kill the dispatcher tasks.
   ColdFrame.Project.Events.Stop (Dispatcher);
   ColdFrame.Project.Events.Tear_Down (Dispatcher);

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));
end Callback_Manager_Test;
