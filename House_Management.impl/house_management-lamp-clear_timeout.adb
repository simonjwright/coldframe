--  $Id: house_management-lamp-clear_timeout.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  This state entry action unsets the instance Timeout.

with ColdFrame.Project.Events;
with House_Management.Events;

separate (House_Management.Lamp)
procedure Clear_Timeout
  (This : Handle) is
begin

   ColdFrame.Project.Events.Unset (The_Timer => This.Timeout,
                                   On => Events.Dispatcher);

end Clear_Timeout;
