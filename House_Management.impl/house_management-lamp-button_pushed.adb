with ColdFrame.Project.Events;
with House_Management.Events;

--  An associated button has been pushed.

separate (House_Management.Lamp)
procedure Button_Pushed
  (This : Handle) is
begin

   ColdFrame.Project.Events.Post (The_Event => new Button_Push (This),
                                  On => Events.Dispatcher);

end Button_Pushed;
