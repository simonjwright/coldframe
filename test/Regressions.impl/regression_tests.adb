--  $Id$
--
--  Regression tests for ColdFrame.

with AUnit.Test_Runner;
with GNAT.Exception_Traces;
with Regressions.Suite;

procedure Regression_Tests is
   procedure Run is new AUnit.Test_Runner (Regressions.Suite.Suite);
begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   Run;

end Regression_Tests;
