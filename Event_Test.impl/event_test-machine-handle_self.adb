with Event_Test.Events;

separate (Event_Test.Machine)
procedure Handle_Self (This : Handle; P : Content) is
   E : ColdFrame.Project.Events.Event_P := new Mark (This);
begin
   This.Ordinal := P.Ordinal;
   Mark (E.all).Payload.Ordinal := This.Ordinal + 1;
   ColdFrame.Project.Events.Post (On => Events.Dispatcher, The_Event => E);
end Handle_Self;
