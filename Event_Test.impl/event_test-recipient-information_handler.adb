separate (Event_Test.Recipient)
procedure Information_Handler
  (I : Information) is
begin
   This.Ordinal := I.Payload.Ordinal;
   This.Offset := Clock - I.Payload.Expected_At;
end Information_Handler;
