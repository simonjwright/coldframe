with ColdFrame.Project.Events;
with Interrupt_Handling.Events;

separate (Interrupt_Handling.Device)
procedure Set_Timeout is
begin

   ColdFrame.Project.Events.Set (This.Timeout,
                                 On => Events.Dispatcher,
                                 To_Fire => new Timeout (This),
                                 After => 5.0);

end Set_Timeout;
