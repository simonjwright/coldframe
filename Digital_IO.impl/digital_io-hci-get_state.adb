with Digital_IO.Signal;

separate (Digital_IO.HCI)
function Get_State
  (Of_Signal : Signal_Name)
  return Boolean is
begin

   return Signal.Get_State (Signal.Find ((S => Of_Signal)));

end Get_State;
