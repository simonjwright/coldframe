separate (Event_Test.Recipient)
procedure Wait_Handler
  (W : Wait) is
begin
   delay W.Payload.Interval;
end Wait_Handler;
