with GNAT.IO; use GNAT.IO;
with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);
with ColdFrame.Events;
with States.Initialize;
with States.Events;
with States.Monitor;
procedure States.T is
   E : ColdFrame.Events.Event_P;
begin
   Initialize;
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Tick;
      Monitor.Tick (E.all).The := Input;
      Put_Line ("generating input Tick");
      ColdFrame.Events.Post (The => E, On => Events.Dispatcher);
   end loop;
   Put_Line ("4 second gap in input Tick");
   delay 4.0;
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Tick;
      Monitor.Tick (E.all).The := Input;
      Put_Line ("generating input Tick");
      ColdFrame.Events.Post (The => E, On => Events.Dispatcher);
   end loop;
   Put_Line ("stopping input Tick");
end States.T;
