with States.Events;
separate (States.Monitor)
procedure Receive
  (M : Tick) is
   E : ColdFrame.Events.Event_P
     := new Monitor.Heartbeat (Find ((Dev => M.The)));
begin
   ColdFrame.Events.Post (The => E, On => Events.Dispatcher);
end Receive;
