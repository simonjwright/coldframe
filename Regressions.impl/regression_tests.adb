--  $Id: regression_tests.adb,v 6e06cee2bcf0 2003/10/31 06:34:19 simon $
--
--  Regression tests for ColdFrame.

with AUnit.Test_Runner;
with Regressions.Suite;

procedure Regression_Tests is
   procedure Run is new AUnit.Test_Runner (Regressions.Suite.Suite);
begin
   Run;
end Regression_Tests;
