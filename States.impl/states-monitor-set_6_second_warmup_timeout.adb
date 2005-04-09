with States.Events;

separate (States.Monitor)
procedure Set_6_Second_Warmup_Timeout (This : Handle) is
begin
   ColdFrame.Project.Events.Set (The_Timer => This.Warmup_Timer,
                                 On => Events.Dispatcher,
                                 To_Fire => new Warmup_Timeout (This),
                                 After => 6.0);
end Set_6_Second_Warmup_Timeout;
