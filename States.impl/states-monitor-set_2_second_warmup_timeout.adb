with States.Events;

separate (States.Monitor)
procedure Set_2_Second_Warmup_Timeout is
   E : ColdFrame.States.Event_P := new Warmup_Timeout (This);
begin
   ColdFrame.States.Set (The => This.Warmup_Timer,
                         On => Events.Dispatcher,
                         To_Fire => E,
                         After => 2.0);
end Set_2_Second_Warmup_Timeout;
