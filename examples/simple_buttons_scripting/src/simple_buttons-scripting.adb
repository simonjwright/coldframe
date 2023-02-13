--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

with ColdFrame.Project.Events.Standard.Test_Trace;
with ColdFrame.Project.Scripted_Testing;
with Digital_IO.Initialize;
with Digital_IO.Scripting;
with Simple_Buttons.Initialize;
with Scripted_Testing;

procedure Simple_Buttons.Scripting is

   procedure Initialize
     (The_Dispatcher : not null ColdFrame.Project.Events.Event_Queue_P);
   procedure Initialize
     (The_Dispatcher : not null ColdFrame.Project.Events.Event_Queue_P)
   is
   begin
      Digital_IO.Initialize (The_Dispatcher);
      Simple_Buttons.Initialize (The_Dispatcher);
   end Initialize;

   Q : constant ColdFrame.Project.Events.Event_Queue_P
     := new ColdFrame.Project.Events.Standard.Test_Trace.Event_Queue;

begin

   ColdFrame.Project.Scripted_Testing.Register
     (The_Dispatcher  => Q,
      With_Initialize => Initialize'Unrestricted_Access);

   Scripted_Testing.Start;

end Simple_Buttons.Scripting;
