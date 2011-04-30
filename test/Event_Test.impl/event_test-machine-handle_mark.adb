separate (Event_Test.Machine)
procedure Handle_Mark
  (This : Handle; P : Content) is
begin
   This.Ordinal := P.Ordinal;
end Handle_Mark;
