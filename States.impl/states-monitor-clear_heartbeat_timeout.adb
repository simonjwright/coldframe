with States.Events;

separate (States.Monitor)
procedure Clear_Heartbeat_Timeout (This : Handle) is
begin
   ColdFrame.Project.Events.Unset (The_Timer => This.Heartbeat_Timer,
                                   On => Events.Dispatcher);
end Clear_Heartbeat_Timeout;
