with States.Events;
separate (States.Monitor)
procedure Receive
  (M : Tick) is
   E : ColdFrame.Project.Events.Event_P
     := new Monitor.Heartbeat (Find ((Dev => M.Payload)));
begin
   ColdFrame.Project.Events.Post (The_Event => E, On => Events.Dispatcher);
end Receive;
