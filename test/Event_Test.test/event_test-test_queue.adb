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

with ColdFrame.Exceptions;
with ColdFrame.Project.Events.Standard.Test;
with ColdFrame.Project.Events.Standard.Test_Trace;
with System;

package body Event_Test.Test_Queue is


   --  You can tear down an Event_Queue that hasn't started (and that
   --  has no references).
   procedure Tear_Down_Unstarted_Queue
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Tear_Down_Unstarted_Queue
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Dispatcher : ColdFrame.Project.Events.Event_Queue_P
        := new ColdFrame.Project.Events.Standard.Event_Queue_Base
        (Start_Started => False,
         Priority => System.Default_Priority,
         Storage_Size => 20_000);
      use type ColdFrame.Project.Events.Event_Queue_P;
   begin
      select
         delay 1.0;
         Assert (R, False, "queue wasn't torn down");
      then abort
         ColdFrame.Project.Events.Stop (Dispatcher);
         ColdFrame.Project.Events.Tear_Down (Dispatcher);
         Assert (R, Dispatcher = null, "dispatcher not nulled");
      end select;
   end Tear_Down_Unstarted_Queue;


   --  A low-priority queue can be started. Note, this error was a
   --  race condition, and can only occur if the OS respects task
   --  priorities (eg, if on Linux, run as root).
   procedure Start_Low_Priority_Queue
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Start_Low_Priority_Queue
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Dispatcher : ColdFrame.Project.Events.Event_Queue_P
        := new ColdFrame.Project.Events.Standard.Event_Queue_Base
        (Start_Started => False,
         Priority => System.Default_Priority - 1,
         Storage_Size => 20_000);
   begin
      select
         delay 1.0;
         Assert (R, False, "queue wasn't started");
      then abort
         ColdFrame.Project.Events.Start (Dispatcher);
         ColdFrame.Project.Events.Stop (Dispatcher);
         ColdFrame.Project.Events.Tear_Down (Dispatcher);
      end select;
   end Start_Low_Priority_Queue;


   --  You can't Wait_Until_Idle on an unstarted queue.
   procedure Wait_Until_Idle_On_Unstarted_Queue
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Wait_Until_Idle_On_Unstarted_Queue
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Dispatcher : ColdFrame.Project.Events.Event_Queue_P
        := new ColdFrame.Project.Events.Standard.Test.Event_Queue_Base
        (Start_Started => False,
         Priority => System.Default_Priority,
         Storage_Size => 20_000);
   begin
      ColdFrame.Project.Events.Wait_Until_Idle (Dispatcher);
      ColdFrame.Project.Events.Stop (Dispatcher);
      ColdFrame.Project.Events.Tear_Down (Dispatcher);
      Assert (R, False, "there was no exception");
   exception
      when ColdFrame.Exceptions.Use_Error =>
         ColdFrame.Project.Events.Stop (Dispatcher);
         ColdFrame.Project.Events.Tear_Down (Dispatcher);
   end Wait_Until_Idle_On_Unstarted_Queue;


   --  A Test_Trace queue starts 'unstarted'.
   procedure Test_Trace_Queue_Starts_Unstarted
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Trace_Queue_Starts_Unstarted
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Dispatcher : ColdFrame.Project.Events.Event_Queue_P
        := new ColdFrame.Project.Events.Standard.Test_Trace.Event_Queue;
   begin
      ColdFrame.Project.Events.Start (Dispatcher);
      ColdFrame.Project.Events.Stop (Dispatcher);
      ColdFrame.Project.Events.Tear_Down (Dispatcher);
   exception
      when ColdFrame.Exceptions.Use_Error =>
         ColdFrame.Project.Events.Stop (Dispatcher);
         ColdFrame.Project.Events.Tear_Down (Dispatcher);
         Assert (R, False, "was started");
   end Test_Trace_Queue_Starts_Unstarted;


   ---------------
   --  Harness  --
   ---------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T,
         Tear_Down_Unstarted_Queue'Access,
         "Unstarted queue can be torn down");
      Registration.Register_Routine
        (T,
         Start_Low_Priority_Queue'Access,
         "Low-priority queue can be started");
      Registration.Register_Routine
        (T,
         Wait_Until_Idle_On_Unstarted_Queue'Access,
         "can't Wait_Until_Idle on unstarted queue");
      Registration.Register_Routine
        (T,
         Test_Trace_Queue_Starts_Unstarted'Access,
         "Test_Trace queue starts unstarted");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Warnings (Off, T);
   begin
      return new String'("Event queue");
   end Name;

end Event_Test.Test_Queue;
