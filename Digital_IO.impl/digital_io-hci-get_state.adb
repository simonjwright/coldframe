--  $Id: digital_io-hci-get_state.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  Returns the state of the specified signal.

with Digital_IO.Signal;

separate (Digital_IO.HCI)
function Get_State
  (Of_Signal : Signal_Name)
  return Boolean is
begin

   return Signal.Get_State (Signal.Find ((S => Of_Signal)));

end Get_State;
