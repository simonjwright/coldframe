--  $Id: digital_io-hci-set_input.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  Sets the specified input signal to the given value.

with Digital_IO.Signal;

separate (Digital_IO.HCI)
procedure Set_Input
  (Of_Signal : Signal_Name;
   To : Boolean) is
begin

   Signal.Set_State (Signal.Find ((S => Of_Signal)), To => To);

end Set_Input;
