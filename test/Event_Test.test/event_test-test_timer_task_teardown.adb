with Event_Test.Initialize;
with Event_Test.Tear_Down;

with Event_Test.Timer_Task_Teardown;
with Event_Test.Events;

with ColdFrame.Project.Events;

package body Event_Test.Test_Timer_Task_Teardown is

   --  Just start the test; shouldn't actually fail
   procedure Run
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Run
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      Timer_Task_Teardown.Start;
      delay 0.5;
      Assert (R, True, "shouldn't fail");
   end Run;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Run'Access, "running");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Warnings (Off, T);
   begin
      return new String'("Timer/task teardown");
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

end Event_Test.Test_Timer_Task_Teardown;
