with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with Event_Test.Initialize;
with Event_Test.Tear_Down;

with Event_Test.Completion_Transitions;
with Event_Test.Events;

with ColdFrame.Project.Events;

package body Event_Test.Test_Completion_Transitions is

   --  complete lifecycle
   procedure Complete_Lifecycle
      (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Complete_Lifecycle
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Assert (Completion_Transitions.Get_Status = 1,
              "incorrect status " & Completion_Transitions.Get_Status'Img);
      ColdFrame.Project.Events.Post
        (The_Event =>
           new Completion_Transitions.E2 (Completion_Transitions.Find),
         On => Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Completion_Transitions.Get_Status = 3,
              "incorrect status " & Completion_Transitions.Get_Status'Img);
   end Complete_Lifecycle;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Complete_Lifecycle'Access, "complete lifecycle");
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Completion transitions");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Initialize;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Tear_Down;
   end Tear_Down;

end Event_Test.Test_Completion_Transitions;
