with AUnit.Test_Runner;
with Event_Test.Suite;

procedure Event_Test.Harness is

   procedure Run is new AUnit.Test_Runner (Suite);

begin

   Run;

end Event_Test.Harness;
