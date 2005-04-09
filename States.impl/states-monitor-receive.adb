with States.Events;
separate (States.Monitor)
procedure Receive
  (M : Tick) is
begin
   ColdFrame.Project.Events.Post
     (The_Event => new Monitor.Heartbeat (Find ((Dev => M.Payload))),
      On => Events.Dispatcher);
end Receive;
