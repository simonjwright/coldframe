with Event_Test.Events;

separate (Event_Test.Recipient)
procedure Handle_Self
  (P : Content) is
   E : constant ColdFrame.Project.Events.Event_P := new Mark (This);
begin
   This.Ordinal := P.Ordinal;
   Mark (E.all).Payload.Ordinal := This.Ordinal + 1;
   ColdFrame.Project.Events.Post (On => Events.Dispatcher, The_Event => E);
end Handle_Self;
