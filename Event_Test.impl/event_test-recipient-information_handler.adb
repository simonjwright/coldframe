with GNAT.IO;

separate (Event_Test.Recipient)
procedure Information_Handler
  (I : Information) is
begin
   This.Ordinal := I.Payload.Ordinal;
   This.Offset := Clock - I.Payload.Expected_At;
   GNAT.IO.Put_Line ("message: ordinal" &
                       This.Ordinal'Img &
                       ", " &
                       This.Offset'Img);
end Information_Handler;
