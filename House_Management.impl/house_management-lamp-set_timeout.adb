with ColdFrame.Project.Events;
with House_Management.Events;

separate (House_Management.Lamp)
procedure Set_Timeout
  (This : Handle) is
begin

   ColdFrame.Project.Events.Set (The_Timer => This.Timeout,
                                 On => Events.Dispatcher,
                                 To_Fire => new Timeout (This),
                                 After => 5.0);

end Set_Timeout;
