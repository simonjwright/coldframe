with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with ColdFrame.Project.Events.Standard;
with System;

package body Event_Test.Test_Queue is


   --  You can tear down an Event_Queue that hasn't started.
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
   begin
      select
         delay 1.0;
         Assert (False, "queue wasn't torn down");
      then abort
         ColdFrame.Project.Events.Stop (Dispatcher);
         ColdFrame.Project.Events.Tear_Down (Dispatcher);
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
         Assert (False, "queue wasn't started");
      then abort
         ColdFrame.Project.Events.Start (Dispatcher);
         ColdFrame.Project.Events.Stop (Dispatcher);
         ColdFrame.Project.Events.Tear_Down (Dispatcher);
      end select;
   end Start_Low_Priority_Queue;


   ---------------
   --  Harness  --
   ---------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T,
         Tear_Down_Unstarted_Queue'Access,
         "Unstarted queue can be torn down");
      Register_Routine
        (T,
         Start_Low_Priority_Queue'Access,
         "Low-priority queue can be started");
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Event queue");
   end Name;

end Event_Test.Test_Queue;
