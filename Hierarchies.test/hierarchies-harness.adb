with AUnit.Test_Runner;

with Hierarchies.Suite;

procedure Hierarchies.Harness is

   procedure Run is new AUnit.Test_Runner (Suite);

begin

   Run;

end Hierarchies.Harness;
