with GNAT.IO; use GNAT.IO;
with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);
with ColdFrame.Project.Events;
with States.Initialize;
with States.Events;
with States.Monitor;
procedure States.T is
   E : ColdFrame.Project.Events.Event_P;
begin
   Initialize;
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Tick;
      Monitor.Tick (E.all).Payload := Input;
      Put_Line ("generating input Tick");
      ColdFrame.Project.Events.Post (The => E, On => Events.Dispatcher);
   end loop;
   Put_Line ("4 second gap in input Tick");
   delay 4.0;
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Tick;
      Monitor.Tick (E.all).Payload := Input;
      Put_Line ("generating input Tick");
      ColdFrame.Project.Events.Post (The => E, On => Events.Dispatcher);
   end loop;
   Put_Line ("stopping input Tick");
end States.T;
