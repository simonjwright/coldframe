with AUnit.Test_Runner;
with Regressions.Suite;

procedure Regression_Tests is
   procedure Run is new AUnit.Test_Runner (Regressions.Suite.Suite);
begin
   Run;
end Regression_Tests;
