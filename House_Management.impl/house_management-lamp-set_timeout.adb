--  $Id: house_management-lamp-set_timeout.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  This state entry action sets the instance Timeout to the required
--  activation period.

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
