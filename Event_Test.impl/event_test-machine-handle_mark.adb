separate (Event_Test.Machine)
procedure Handle_Mark
  (This : Handle; Ev : Mark) is
begin
   This.Ordinal := Ev.Payload.Ordinal;
end Handle_Mark;
