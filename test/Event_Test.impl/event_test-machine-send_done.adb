with Event_Test.Events;

separate (Event_Test.Machine)
procedure Send_Done (This : Handle) is
begin
   This.Ordinal := 0;
   ColdFrame.Project.Events.Post_To_Self
     (On => Events.Dispatcher, The_Event => new Done (This));
end Send_Done;
