with Digital_IO.Signal;

separate (Digital_IO.HCI)
procedure Set_Input
  (Of_Signal : Signal_Name;
   To : Boolean) is
begin

   Signal.Set_State (Signal.Find ((S => Of_Signal)), To => To);

end Set_Input;
