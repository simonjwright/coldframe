--  $Id: digital_io-application-set_output.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  Sets the specified output signal to the given state.

with Digital_IO.Signal;

separate (Digital_IO.Application)
procedure Set_Output
  (S : Signal_Name;
   To_State : Boolean) is
begin

   Signal.Set_State (Signal.Find ((S => S)), To => To_State);

end Set_Output;
