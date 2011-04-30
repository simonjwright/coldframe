with AUnit.Test_Runner;
with Initialization.Suite;
with GNAT.Exception_Traces;

procedure Initialization.Harness is

   procedure Run is new AUnit.Test_Runner (Suite.Suite);

begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   Run;

end Initialization.Harness;

