with States.Events;

separate (States.Monitor)
procedure Set_Heartbeat_Timeout (This : Handle) is
   E : ColdFrame.Project.Events.Event_P := new Heartbeat_Timeout (This);
begin
   ColdFrame.Project.Events.Set (The_Timer => This.Heartbeat_Timer,
                                 On => Events.Dispatcher,
                                 To_Fire => E,
                                 After => 3.0);
end Set_Heartbeat_Timeout;
