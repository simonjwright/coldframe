with Ada.Text_IO;
with AUnit.Test_Runner;
with Event_Test.Suite;
with GNAT.Exception_Traces;

procedure Event_Test.Harness is

   procedure Run is new AUnit.Test_Runner (Suite);

begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   Ada.Text_IO.Put_Line ("This test suite takes approximately 35 seconds.");

   Run;

end Event_Test.Harness;
