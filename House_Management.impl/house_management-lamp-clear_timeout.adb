with ColdFrame.Project.Events;
with House_Management.Events;

separate (House_Management.Lamp)
procedure Clear_Timeout
  (This : Handle) is
begin

   ColdFrame.Project.Events.Unset (The_Timer => This.Timeout,
                                   On => Events.Dispatcher);

end Clear_Timeout;
