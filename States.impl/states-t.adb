with GNAT.IO; use GNAT.IO;
with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);
with ColdFrame.States;
with States.Initialize;
with States.Events;
with States.Monitor;
procedure States.T is
   E : ColdFrame.States.Event_P;
   M : Monitor.Handle;
begin
   Initialize;
   M := Monitor.Find;
   E := new Monitor.Start (M);
   Put_Line ("Generating Start");
   ColdFrame.States.Post (It => E.all, On => Events.Dispatcher);
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Heartbeat (M);
      Put_Line ("generating Heartbeat");
      ColdFrame.States.Post (It => E.all, On => Events.Dispatcher);
   end loop;
   Put_Line ("4 second gap in Heartbeat");
   delay 4.0;
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Heartbeat (M);
      Put_Line ("generating Heartbeat");
      ColdFrame.States.Post (It => E.all, On => Events.Dispatcher);
   end loop;
   Put_Line ("stopping Heartbeat");
end States.T;
