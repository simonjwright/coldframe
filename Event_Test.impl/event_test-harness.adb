with AUnit.Test_Runner;
with Event_Test.Suite;
with Ada.Task_Identification;

procedure Event_Test.Harness is

   procedure Run is new AUnit.Test_Runner (Suite);

begin

   Run;

   Ada.Task_Identification.Abort_Task (Ada.Task_Identification.Current_Task);

end Event_Test.Harness;
