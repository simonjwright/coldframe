--  Regression tests for ColdFrame.

with AUnit.Reporter.Text;
with AUnit.Run;
with GNAT.Exception_Traces;
with Regressions.Suite;

procedure Regression_Tests is
   procedure Run is new AUnit.Run.Test_Runner (Regressions.Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   Run (Reporter);

end Regression_Tests;
