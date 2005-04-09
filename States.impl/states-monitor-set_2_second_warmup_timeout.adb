with States.Events;

separate (States.Monitor)
procedure Set_2_Second_Warmup_Timeout (This : Handle) is
begin
   ColdFrame.Project.Events.Set (The_Timer => This.Warmup_Timer,
                                 On => Events.Dispatcher,
                                 To_Fire => new Warmup_Timeout (This),
                                 After => 2.0);
end Set_2_Second_Warmup_Timeout;
