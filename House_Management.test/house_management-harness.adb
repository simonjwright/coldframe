--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  $RCSfile: house_management-harness.adb,v $
--  $Revision: 9390212d4875 $
--  $Date: 2005/09/08 05:41:33 $
--  $Author: simonjwright $

with AUnit.Test_Runner;
with House_Management.Test_Suite;
with GNAT.Exception_Traces;

procedure House_Management.Harness is

   procedure Run is new AUnit.Test_Runner (Test_Suite.Suite);

begin

   GNAT.Exception_Traces.Trace_On
     (Kind => GNAT.Exception_Traces.Unhandled_Raise);

   Run;

end House_Management.Harness;
