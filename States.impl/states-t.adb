with GNAT.IO; use GNAT.IO;
with ColdFrame.Exceptions.Traceback;
with ColdFrame.States.Events;
with States.Initialize;
with States.Monitor;
procedure States.T is
   E : ColdFrame.States.Event_P;
begin
   Initialize;
   E := new Monitor.Start (Monitor.Find);
   Put_Line ("Generating Start");
   ColdFrame.States.Events.Post (E.all);
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Heartbeat (Monitor.Find);
      Put_Line ("generating Heartbeat");
      ColdFrame.States.Events.Post (E.all);
   end loop;
   delay 4.0;
   for N in 1 .. 10 loop
      delay 1.0;
      E := new Monitor.Heartbeat (Monitor.Find);
      Put_Line ("generating Heartbeat");
      ColdFrame.States.Events.Post (E.all);
   end loop;
end States.T;
