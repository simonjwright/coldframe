with States.Events;

separate (States.Monitor)
procedure Set_Heartbeat_Timeout is
   E : ColdFrame.States.Event_P := new Heartbeat_Timeout (This);
begin
   ColdFrame.States.Set (The => This.Heartbeat_Timer,
                         On => Events.Dispatcher,
                         To_Fire => E,
                         After => 3.0);
end Set_Heartbeat_Timeout;
