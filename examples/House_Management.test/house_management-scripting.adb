--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

with ColdFrame.Project.Events.Standard.Test_Trace;
with ColdFrame.Project.Scripted_Testing;
with ColdFrame.Stubs;
with Digital_IO.Initialize;
with Digital_IO.Scripting;
with House_Management.Initialize;
with Scripted_Testing;

procedure House_Management.Scripting is

   Q : constant ColdFrame.Project.Events.Event_Queue_P
     := new ColdFrame.Project.Events.Standard.Test_Trace.Event_Queue;

begin

   ColdFrame.Stubs.Set_Up;
   Digital_IO.Initialize (Q);
   House_Management.Initialize (Q);

   ColdFrame.Project.Scripted_Testing.Register (Q);

   Scripted_Testing.Start;

end House_Management.Scripting;
