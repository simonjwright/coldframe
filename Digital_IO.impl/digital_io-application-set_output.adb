with Digital_IO.Signal;

separate (Digital_IO.Application)
procedure Set_Output
  (S : Signal_Name;
   To_State : Boolean) is
begin

   Signal.Set_State (Signal.Find ((S => S)), To => To_State);

end Set_Output;
