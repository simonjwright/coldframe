with Digital_IO.Signal_State_Callback;

separate (Digital_IO.Input)
procedure Changed
  (This : Handle) is
begin

   Signal_State_Callback.Call_Callbacks
     (With_Param => (S => Get_S (This),
                     State => Get_State (This)));

end Changed;
