separate (Digital_IO)
procedure Set
  (O : Output_Signal;
   To_State : Boolean) is
begin
   Digital_IO_Support.Set (Digital_IO_Support.Output_Signal (O),
                           To => To_State);
end Set;
