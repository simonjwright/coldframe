--  $Id: house_management-lamp-button_pushed.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  An associated button has been pushed.

with ColdFrame.Project.Events;
with House_Management.Events;

separate (House_Management.Lamp)
procedure Button_Pushed
  (This : Handle) is
begin

   ColdFrame.Project.Events.Post (The_Event => new Button_Push (This),
                                  On => Events.Dispatcher);

end Button_Pushed;
