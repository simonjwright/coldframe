separate (Event_Test.Recipient)
procedure Information_Handler
  (I : Information) is
   use type ColdFrame.Project.Calendar.Time;
begin
   This.Ordinal := I.Payload.Ordinal;
   This.Offset := ColdFrame.Project.Calendar.Clock - I.Payload.Expected_At;
end Information_Handler;
