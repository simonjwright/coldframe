--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

with AUnit.Reporter.Text;
with AUnit.Run;

with House_Management.Lamp.Test_Suite;
with GNAT.Exception_Traces;

procedure House_Management.Harness is

   procedure Run is new AUnit.Run.Test_Runner (Lamp.Test_Suite.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   Run (Reporter);

end House_Management.Harness;
