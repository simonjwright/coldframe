with States.Events;

separate (States.Monitor)
procedure Set_6_Second_Warmup_Timeout (This : Handle) is
   E : ColdFrame.Project.Events.Event_P := new Warmup_Timeout (This);
begin
   ColdFrame.Project.Events.Set (The => This.Warmup_Timer,
                                 On => Events.Dispatcher,
                                 To_Fire => E,
                                 After => 6.0);
end Set_6_Second_Warmup_Timeout;
