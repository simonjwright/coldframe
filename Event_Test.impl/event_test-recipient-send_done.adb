with Event_Test.Events;

separate (Event_Test.Recipient)
procedure Send_Done is
begin
   This.Ordinal := 0;
   ColdFrame.Project.Events.Post_To_Self
     (On => Events.Dispatcher, The_Event => new Done (This));
end Send_Done;
