--  $Id: digital_io-input-changed.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  Called when the signal has changed state; notify any registered
--  observers of Signal State.

with Digital_IO.Signal_State_Callback;

separate (Digital_IO.Input)
procedure Changed
  (This : Handle) is
begin

   Signal_State_Callback.Call_Callbacks
     (With_Param => (S => Get_S (This),
                     State => Get_State (This)));

end Changed;
