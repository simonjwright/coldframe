with ColdFrame.Project.Events;
with Interrupt_Handling.Events;

separate (Interrupt_Handling.Device)
procedure Clear_Timeout is
begin

   ColdFrame.Project.Events.Unset (This.Timeout,
                                   On => Events.Dispatcher);

end Clear_Timeout;
