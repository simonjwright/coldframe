with States.Events;

separate (States.Monitor)
procedure Set_Heartbeat_Timeout (This : Handle) is
begin
   ColdFrame.Project.Events.Set (The_Timer => This.Heartbeat_Timer,
                                 On => Events.Dispatcher,
                                 To_Fire => new Heartbeat_Timeout (This),
                                 After => 3.0);
end Set_Heartbeat_Timeout;
