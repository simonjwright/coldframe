with States.Events;

separate (States.Monitor)
procedure Clear_Heartbeat_Timeout (This : Handle) is
begin
   ColdFrame.Events.Unset (The => This.Heartbeat_Timer,
                           On => Events.Dispatcher);
end Clear_Heartbeat_Timeout;
