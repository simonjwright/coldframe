with States.Events;

separate (States.Monitor)
procedure Set_2_Second_Warmup_Timeout (This : Handle) is
   E : ColdFrame.Project.Events.Event_P := new Warmup_Timeout (This);
begin
   ColdFrame.Project.Events.Set (The_Timer => This.Warmup_Timer,
                                 On => Events.Dispatcher,
                                 To_Fire => E,
                                 After => 2.0);
end Set_2_Second_Warmup_Timeout;
