with ColdFrame.Project.Events;
with Regressions.Events;

separate (Regressions.Event_Holder)
procedure Set_Timer
  (This : Handle) is
begin
   ColdFrame.Project.Events.Set (The_Timer => This.T,
                                 On => Events.Dispatcher,
                                 To_Fire => new Timeout (This),
                                 After => 10.0);
end Set_Timer;
